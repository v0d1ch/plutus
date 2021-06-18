{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           PlutusPrelude                            (through)

import           Common
import qualified PlutusCore                               as PLC
import qualified PlutusCore.CBOR                          as PLC
import qualified PlutusCore.Evaluation.Machine.Ck         as Ck
import           PlutusCore.Evaluation.Machine.ExBudget   (ExBudget (..), ExRestrictingBudget (..))
import           PlutusCore.Evaluation.Machine.ExMemory   (ExCPU (..), ExMemory (..))
import qualified PlutusCore.Generators                    as Gen
import qualified PlutusCore.Generators.Interesting        as Gen
import qualified PlutusCore.Generators.Test               as Gen
import qualified PlutusCore.Pretty                        as PP
import           PlutusCore.Rename                        (rename)
import qualified PlutusCore.StdLib.Data.Bool              as StdLib
import qualified PlutusCore.StdLib.Data.ChurchNat         as StdLib
import qualified PlutusCore.StdLib.Data.Integer           as StdLib
import qualified PlutusCore.StdLib.Data.Unit              as StdLib

import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Check.Uniques          (checkProgram)
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek
import qualified UntypedPlutusCore.Parser                 as UPLC (parseProgram)

import           Codec.Serialise
import           Control.DeepSeq                          (NFData, rnf)
import           Control.Monad                            (void)
import           Control.Monad.Trans.Except               (runExcept, runExceptT)
import           Data.Bifunctor                           (second)
import qualified Data.ByteString.Lazy                     as BSL
import           Data.Foldable                            (asum, traverse_)
import           Data.Function                            ((&))
import qualified Data.HashMap.Monoidal                    as H
import           Data.List                                (nub)
import qualified Data.List                                as List
import           Data.List.Split                          (splitOn)
import qualified Data.Text                                as T
import           Data.Text.Encoding                       (encodeUtf8)
import qualified Data.Text.IO                             as T
import           Data.Text.Prettyprint.Doc                (Doc, pretty, (<+>))
import           Data.Traversable                         (for)
import           Flat                                     (Flat, flat, unflat)
import           Options.Applicative
import           System.CPUTime                           (getCPUTime)
import           System.Exit                              (exitFailure, exitSuccess)
import           System.Mem                               (performGC)
import           Text.Printf                              (printf)
import           Text.Read                                (readMaybe)

-- | Untyped AST with names consisting solely of De Bruijn indices. This is
-- currently only used for intermediate values during CBOR/Flat
-- serialisation/deserialisation.  We may wish to add TypedProgramDeBruijn as
-- well if we modify the CEK machine to run directly on de Bruijnified ASTs, but
-- support for this is lacking elsewhere at the moment.
type UntypedProgramDeBruijn a = UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun a

uplcHelpText :: String
uplcHelpText = helpText "Untyped Plutus Core"

uplcInfoCommand :: ParserInfo Command
uplcInfoCommand = plutus "Untyped Plutus Core Tool" uplcHelpText

---------------- Name conversions ----------------

-- | Convert an untyped de-Bruijn-indexed program to one with standard names.
-- We have nothing to base the names on, so every variable is named "v" (but
-- with a Unique for disambiguation).  Again, we don't support typed programs.
fromDeBruijn :: UntypedProgramDeBruijn a -> IO (UntypedProgram a)
fromDeBruijn prog = do
    let namedProgram = UPLC.programMapNames (\(UPLC.DeBruijn ix) -> UPLC.NamedDeBruijn "v" ix) prog
    case PLC.runQuote $ runExceptT @UPLC.FreeVariableError $ UPLC.unDeBruijnProgram namedProgram of
      Left e  -> errorWithoutStackTrace $ show e
      Right p -> return p

-- | Convert an untyped program to one where the 'name' type is de Bruijn indices.
toDeBruijn :: UntypedProgram a -> IO (UntypedProgramDeBruijn a)
toDeBruijn prog =
  case runExcept @UPLC.FreeVariableError (UPLC.deBruijnProgram prog) of
    Left e  -> errorWithoutStackTrace $ show e
    Right p -> return $ UPLC.programMapNames (\(UPLC.NamedDeBruijn _ ix) -> UPLC.DeBruijn ix) p

-- | Convert names to de Bruijn indices and then serialise
serialiseDbProgramCBOR :: Program () -> IO BSL.ByteString
serialiseDbProgramCBOR (UntypedProgram p) = UPLC.serialiseOmittingUnits <$> toDeBruijn p

-- | Convert names to de Bruijn indices and then serialise
serialiseDbProgramFlat :: Flat a => Program a -> IO BSL.ByteString
serialiseDbProgramFlat (UntypedProgram p) = BSL.fromStrict . flat <$> toDeBruijn p

parseUplcInput = parseInput UPLC.parseProgram checkProgram

loadASTfromCBOR :: AstNameType -> Input -> IO (Program ())
loadASTfromCBOR cborMode inp =
    case cborMode of
         Named    -> getBinaryInput inp >>= handleResult UntypedProgram . UPLC.deserialiseRestoringUnitsOrFail
         DeBruijn -> getBinaryInput inp >>=
                                   mapM fromDeBruijn . UPLC.deserialiseRestoringUnitsOrFail >>= handleResult UntypedProgram
    where handleResult wrapper =
              \case
               Left (DeserialiseFailure offset msg) ->
                   errorWithoutStackTrace $ "CBOR deserialisation failure at offset " ++ Prelude.show offset ++ ": " ++ msg
               Right r -> return $ wrapper r

-- Read and deserialise a Flat-encoded AST
loadASTfromFlat :: AstNameType -> Input -> IO (Program ())
loadASTfromFlat flatMode inp =
    case flatMode of
         Named                  -> getBinaryInput inp >>= handleResult TypedProgram . unflat
         (UntypedPLC, Named)    -> getBinaryInput inp >>= handleResult UntypedProgram . unflat
         DeBruijn               -> typedDeBruijnNotSupportedError
         (UntypedPLC, DeBruijn) -> getBinaryInput inp >>= mapM fromDeBruijn . unflat >>= handleResult UntypedProgram
    where handleResult wrapper =
              \case
               Left e  -> errorWithoutStackTrace $ "Flat deserialisation failure: " ++ show e
               Right r -> return $ wrapper r

---------------- Reading programs from files ----------------

parseUplcInput :: Input
  -> IO
     (UPLC.Program
        PLC.Name PLC.DefaultUni PLC.DefaultFun PLC.AlexPosn)
parseUplcInput = parseInput UPLC.parseProgram checkProgram

-- Read either a PLC file or a CBOR file, depending on 'fmt'
getProgram :: Format -> Input  -> IO (Program PLC.AlexPosn)
getProgram fmt inp =
    case fmt of
      Plc  -> parseUplcInput inp
      Cbor cborMode -> do
               prog <- loadASTfromCBOR cborMode inp
               return $ PLC.AlexPn 0 0 0 <$ prog  -- No source locations in CBOR, so we have to make them up.
      Flat flatMode -> do
               prog <- loadASTfromFlat flatMode inp
               return $ PLC.AlexPn 0 0 0 <$ prog  -- No source locations in CBOR, so we have to make them up.

-- | Apply one script to a list of others.
runApply :: ApplyOptions -> IO ()
runApply (ApplyOptions inputfiles ifmt outp ofmt mode) = do
  scripts <- mapM (getProgram language ifmt . FileInput) inputfiles
  let appliedScript =
        case map (\case UntypedProgram p -> () <$ p; _ -> error "unexpected program type mismatch") scripts of
            []          -> errorWithoutStackTrace "No input files"
            progAndArgs -> UntypedProgram $ foldl1 UPLC.applyProgram progAndArgs
writeProgram outp ofmt mode appliedScript


-- TODO: This supplies both typed and untyped examples.  Currently the untyped
-- examples are obtained by erasing typed ones, but it might be useful to have
-- some untyped ones that can't be obtained by erasure.
getAvailableExamples :: IO [(ExampleName, SomeExample)]
getAvailableExamples = do
    interesting <- getInteresting
    let examples = simpleExamples ++ map (second $ SomeTypedTermExample . toTypedTermExample) interesting
    pure $ mapMaybeSnd convert examples
        where convert =
                    \case
                        SomeTypeExample _ -> Nothing
                        SomeTypedTermExample (TypedTermExample _ e) ->
                            Just . SomeUntypedExample . SomeUntypedTermExample . UntypedTermExample $ UPLC.erase e
              mapMaybeSnd _ []     = []
              mapMaybeSnd f ((a,b):r) =
                case f b of
                  Nothing -> mapMaybeSnd f r
                  Just b' -> (a,b') : mapMaybeSnd f r

---------------- Evaluation ----------------

runEval :: EvalOptions -> IO ()
runEval (EvalOptions inp ifmt evalMode printMode budgetMode timingMode cekModel) =
    case evalMode of
    CK  -> errorWithoutStackTrace "There is no CK machine for Untyped Plutus Core"
    CEK -> do
            UntypedProgram prog <- getProgram UntypedPLC ifmt inp
            let term = void . UPLC.toTerm $ prog
                !_ = rnf term
                cekparams = case cekModel of
                        Default -> PLC.defaultCekParameters  -- AST nodes are charged according to the default cost model
                        Unit    -> PLC.unitCekParameters     -- AST nodes are charged one unit each, so we can see how many times each node
                                                                -- type is encountered.  This is useful for calibrating the budgeting code.
            case budgetMode of
              Silent -> do
                      let evaluate = Cek.evaluateCekNoEmit cekparams
                      case timingMode of
                        NoTiming -> evaluate term & handleResult
                        Timing n -> timeEval n evaluate term >>= handleTimingResults term
              Verbose bm -> do
                      let evaluate = Cek.runCekNoEmit cekparams bm
                      case timingMode of
                        NoTiming -> do
                                let (result, budget) = evaluate term
                                printBudgetState term cekModel budget
                                handleResultSilently result  -- We just want to see the budget information
                        Timing n -> timeEval n evaluate term >>= handleTimingResultsWithBudget term

runUplcPrint :: PrintOptions -> IO ()
runUplcPrint = runPrint parseUplcInput


---------------- Printing budgets and costs ----------------

printBudgetStateBudget :: UPLC.Term UPLC.Name PLC.DefaultUni PLC.DefaultFun () -> CekModel -> ExBudget -> IO ()
printBudgetStateBudget _ model b =
    case model of
      Unit -> pure ()
      _ ->  let ExCPU cpu = _exBudgetCPU b
                ExMemory mem = _exBudgetMemory b
            in do
              putStrLn $ "CPU budget:    " ++ show cpu
              putStrLn $ "Memory budget: " ++ show mem

printBudgetStateTally :: (Eq fun, Cek.Hashable fun, Show fun)
       => UPLC.Term UPLC.Name PLC.DefaultUni PLC.DefaultFun () -> CekModel ->  Cek.CekExTally fun -> IO ()
printBudgetStateTally term model (Cek.CekExTally costs) = do
  putStrLn $ "Const      " ++ pbudget (Cek.BStep Cek.BConst)
  putStrLn $ "Var        " ++ pbudget (Cek.BStep Cek.BVar)
  putStrLn $ "LamAbs     " ++ pbudget (Cek.BStep Cek.BLamAbs)
  putStrLn $ "Apply      " ++ pbudget (Cek.BStep Cek.BApply)
  putStrLn $ "Delay      " ++ pbudget (Cek.BStep Cek.BDelay)
  putStrLn $ "Force      " ++ pbudget (Cek.BStep Cek.BForce)
  putStrLn $ "Builtin    " ++ pbudget (Cek.BStep Cek.BBuiltin)
  putStrLn ""
  putStrLn $ "startup    " ++ pbudget Cek.BStartup
  putStrLn $ "compute    " ++ printf "%-20s" (budgetToString totalComputeCost)
  putStrLn $ "AST nodes  " ++ printf "%15d" (UPLC.termSize term)
  putStrLn ""
  putStrLn $ "BuiltinApp " ++ budgetToString builtinCosts
  case model of
    Default ->
        do
  -- 1e9*(0.200  + 0.0000725 * totalComputeSteps + builtinExeTimes/1000)  putStrLn ""
          putStrLn ""
          traverse_ (\(b,cost) -> putStrLn $ printf "%-20s %s" (show b) (budgetToString cost :: String)) builtinsAndCosts
          putStrLn ""
          putStrLn $ "Total budget spent: " ++ printf (budgetToString totalCost)
          putStrLn $ "Predicted execution time: " ++ formatTimePicoseconds totalTime
    Unit -> pure ()
  where
        getSpent k =
            case H.lookup k costs of
              Just v  -> v
              Nothing -> ExBudget 0 0
        allNodeTags = fmap Cek.BStep [Cek.BConst, Cek.BVar, Cek.BLamAbs, Cek.BApply, Cek.BDelay, Cek.BForce, Cek.BBuiltin]
        totalComputeCost = mconcat $ map getSpent allNodeTags  -- For unitCekCosts this will be the total number of compute steps
        budgetToString (ExBudget (ExCPU cpu) (ExMemory mem)) =
            printf "%15s  %15s" (show cpu) (show mem) :: String -- Not %d: doesn't work when CostingInteger is SatInt.
        pbudget = budgetToString . getSpent
        f l e = case e of {(Cek.BBuiltinApp b, cost)  -> (b,cost):l; _ -> l}
        builtinsAndCosts = List.foldl f [] (H.toList costs)
        builtinCosts = mconcat (map snd builtinsAndCosts)
        -- ^ Total builtin evaluation time (according to the models) in picoseconds (units depend on BuiltinCostModel.costMultiplier)
        getCPU b = let ExCPU b' = _exBudgetCPU b in fromIntegral b'::Double
        totalCost = getSpent Cek.BStartup <> totalComputeCost <> builtinCosts
        totalTime = getCPU (getSpent Cek.BStartup) + getCPU totalComputeCost + getCPU builtinCosts

class PrintBudgetState cost where
    printBudgetState :: UPLC.Term PLC.Name PLC.DefaultUni PLC.DefaultFun () -> CekModel -> cost -> IO ()
    -- TODO: Tidy this up.  We're passing in the term and the CEK cost model
    -- here, but we only need them in tallying mode (where we need the term so
    -- we can print out the AST size and we need the model type to decide how
    -- much information we're going to print out).

instance PrintBudgetState Cek.CountingSt where
    printBudgetState term model (Cek.CountingSt budget) = printBudgetStateBudget term model budget

instance (Eq fun, Cek.Hashable fun, Show fun) => PrintBudgetState (Cek.TallyingSt fun) where
    printBudgetState term model (Cek.TallyingSt tally budget) = do
        printBudgetStateBudget term model budget
        putStrLn ""
        printBudgetStateTally term model tally

instance PrintBudgetState Cek.RestrictingSt where
    printBudgetState term model (Cek.RestrictingSt (ExRestrictingBudget budget)) =
        printBudgetStateBudget term model budget




main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) plcInfoCommand
    case options of
        Apply     opts -> runApply        opts
        Typecheck opts -> runTypecheck    opts
        Eval      opts -> runEval         opts
        Example   opts -> runPrintExample opts
        Erase     opts -> runErase        opts
        Print     opts -> runUplcPrint        opts
        Convert   opts -> runConvert      opts
