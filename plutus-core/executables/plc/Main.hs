module Main (main) where


plcHelpText :: String
plcHelpText = helpText "Typed Plutus Core"

plcInfoCommand :: ParserInfo Command
plcInfoCommand = plutus "Typed Plutus Core Tool" plcHelpText

---------------- Reading programs from files ----------------

parsePlcInput :: Input
  -> IO
     (PLC.Program
        PLC.TyName PLC.Name PLC.DefaultUni PLC.DefaultFun PLC.AlexPosn)
parsePlcInput = parseInput PLC.parseProgram checkProgram

-- | Print out a PLC program in IO.
writePlc ::
  Output ->
    PrintMode
    -> (PLC.Program PLC.TyName PLC.Name PLC.DefaultUni PLC.DefaultFun) a -> IO ()
writePlc outp mode prog = do
  let printMethod = getPrintMethod mode
  case outp of
        FileOutput file -> writeFile file . Prelude.show . printMethod $ prog
        StdOutput       -> print . printMethod $ prog

-- | Print out a UPLC program to IO.
writeUplc :: Output -> PrintMode -> UPLC.Program a -> IO ()
writeUplc outp mode prog = do
  let printMethod = getPrintMethod mode
  case outp of
        FileOutput file -> writeFile file . Prelude.show . printMethod $ prog
        StdOutput       -> print . printMethod $ prog

writeProgram :: Output -> Format -> PrintMode -> Program a -> IO ()
writeProgram outp Plc mode prog   = writePlc outp mode prog
writeProgram outp (Cbor _) _ prog = writeCBOR outp prog
writeProgram outp (Flat _) _ prog = writeFlat outp prog
runPlcPrint :: PrintOptions -> IO ()
runPlcPrint = runPrint parsePlcInput

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
