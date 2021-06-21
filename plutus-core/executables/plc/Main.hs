module Main (main) where

import           Common
import qualified UntypedPlutusCore.Core.Type as UPLC

plcHelpText :: String
plcHelpText = helpText "Typed Plutus Core"

plcInfoCommand :: ParserInfo Command
plcInfoCommand = plutus "Typed Plutus Core Tool" plcHelpText

---------------- Erasure ----------------

-- | Input a program, erase the types, then output it
runErase :: EraseOptions -> IO ()
runErase (EraseOptions inp ifmt outp ofmt mode) = do
  typedProg <- getProgram ifmt inp
  let untypedProg = () <$ UPLC.eraseProgram typedProg
  case ofmt of
    Plc    -> writeUplc outp mode untypedProg
    Cbor _ -> writeCBOR outp untypedProg
    Flat _ -> writeFlat outp untypedProg


---------------- Typechecking ----------------

runTypecheck :: TypecheckOptions -> IO ()
runTypecheck (TypecheckOptions inp fmt) = do
  prog <- getProgram fmt inp
  case PLC.runQuoteT $ do
    tcConfig <- PLC.getDefTypeCheckConfig ()
    PLC.typecheckPipeline tcConfig (void prog)
    of
      Left (e :: PLC.Error PLC.DefaultUni PLC.DefaultFun ()) ->
        errorWithoutStackTrace $ PP.displayPlcDef e
      Right ty                                               ->
        T.putStrLn (PP.displayPlcDef ty) >> exitSuccess

---------------- Driver ----------------
-- TODO will be done via type instance?
main :: IO ()
main = do
    options <- customExecParser (prefs showHelpOnEmpty) plcInfoCommand
    case options of
        Apply     opts -> runApply        opts
        Typecheck opts -> runTypecheck    opts
        Eval      opts -> runEval         opts
        Example   opts -> runPrintExample opts
        Erase     opts -> runErase        opts
        Print     opts -> runPlcPrint        opts
        Convert   opts -> runConvert      opts
