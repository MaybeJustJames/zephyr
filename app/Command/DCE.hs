-- | Dead code elimination command based on `Language.PureScript.CoreFn.DCE`.
module Command.DCE
  ( command
  , runDCECommand
  , dceOptions
  , entryPointOpt
  ) where

import           Control.Monad
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Supply
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Writer
import qualified Data.Aeson as A
import           Data.Aeson.Internal (JSONPath)
import           Data.Aeson.Internal as A
import           Data.Aeson.Parser (eitherDecodeWith, json)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson.Types (Value)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as B (toStrict, fromStrict)
import qualified Data.ByteString.UTF8 as BU8
import           Data.Bool (bool)
import           Data.Either (Either, lefts, rights)
import           Data.List (null)
import           Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Encoding as TE
import           Data.Version (Version)
import qualified Language.JavaScript.Parser as JS
import qualified Language.PureScript as P
import qualified Language.PureScript.Bundle as P
import qualified Language.PureScript.CodeGen.JS as P
import qualified Language.PureScript.CodeGen.JS.Printer as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
import qualified Language.PureScript.CoreFn.ToJSON as CoreFn
import qualified Language.PureScript.CoreImp.AST as Imp
import           Language.PureScript.DCE
import qualified Language.PureScript.Errors.JSON as P
import qualified Options.Applicative as Opts
import           System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory)
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>), takeDirectory)
import           System.FilePath.Glob (compile, globDir1)
import           System.IO (hPutStrLn, stderr)
import qualified System.IO as IO

printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    IO.hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      IO.hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  IO.hPutStrLn stderr . BU8.toString . B.toStrict . A.encode $
    P.JSONResult (P.toJSONErrors verbose P.Warning warnings)
               (either (P.toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

inputDirectoryOpt :: Opts.Parser FilePath
inputDirectoryOpt = Opts.strOption $
     Opts.short 'i'
  <> Opts.long "input-directory"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "Input directory (purs output directory)."

outputDirectoryOpt :: Opts.Parser FilePath
outputDirectoryOpt = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "dce-output"
  <> Opts.value "dce-output"
  <> Opts.showDefault
  <> Opts.help "Output directory."

entryPointOpt :: Opts.Parser EntryPoint
entryPointOpt = Opts.argument (Opts.auto >>= checkIfQualified) $
     Opts.metavar "entry-point"
  <> Opts.help "Qualified identifier. All code which is not a transitive dependency of an entry point will be removed. You can pass multiple entry points."
  where
  checkIfQualified (EntryPoint q@(P.Qualified Nothing _)) = fail $
    "not a qualified indentifier: '" ++ T.unpack (P.showQualified P.runIdent q) ++ "'"
  checkIfQualified e = return e

dumpCoreFnOpt :: Opts.Parser Bool
dumpCoreFnOpt = Opts.switch $
     Opts.long "dump-corefn"
  <> Opts.showDefault
  <> Opts.help "Dump the (functional) core representation of the dce-ed."

verboseOutputOpt :: Opts.Parser Bool
verboseOutputOpt = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose"
  <> Opts.showDefault
  <> Opts.help "Verbose CoreFn parser errors."

dceForeignOpt :: Opts.Parser Bool
dceForeignOpt = Opts.switch $
     Opts.short 'f'
  <> Opts.long "dce-foreign"
  <> Opts.showDefault
  <> Opts.help "dce foriegn modules"

dceOptions :: Opts.Parser DCEOptions
dceOptions = DCEOptions
  <$> Opts.some entryPointOpt
  <*> inputDirectoryOpt
  <*> outputDirectoryOpt
  <*> dumpCoreFnOpt
  <*> verboseOutputOpt
  <*> dceForeignOpt

readInput :: [FilePath] -> IO [Either (FilePath, JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)]
readInput inputFiles = forM inputFiles (\f -> addPath f . decodeCoreFn <$> B.readFile f)
  where
  decodeCoreFn :: B.ByteString -> Either (JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)
  decodeCoreFn = eitherDecodeWith json (A.iparse CoreFn.moduleFromJSON) . B.fromStrict

  addPath
    :: FilePath
    -> Either (JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)
    -> Either (FilePath, JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)
  addPath f = either (Left . incl) Right
    where incl (l,r) = (f,l,r)

data DCEAppError
  = ParseErrors [Text]
  | InputNotDirectory FilePath
  | NoInputs FilePath
  | CompilationError (DCEError 'Error)

formatDCEAppError :: FilePath -> DCEAppError -> String
formatDCEAppError _ (ParseErrors errs)
  = colorString errorColor "Error" ++ "\nFailed parsing:\n  " ++ T.unpack (T.intercalate "\n\t" errs)
formatDCEAppError _ (NoInputs path)
  = colorString errorColor "Error" ++ "\nNo inputs found under " ++ colorString codeColor path ++ " directory."
formatDCEAppError _ (InputNotDirectory path)
  = colorString errorColor "Error" ++ "\nDirectory " ++ colorString codeColor path ++ " does not exist."
formatDCEAppError relPath (CompilationError err)
  = displayDCEError relPath err

dceCommand :: DCEOptions -> ExceptT DCEAppError IO ()
dceCommand opts = do
    let entryPoints = runEntryPoint <$> dceEntryPoints opts
        cfnGlb = compile "**/corefn.json"
    inpts <- liftIO $ globDir1 cfnGlb (dceInputDir opts) >>= readInput

    inptDirExist <- lift $ doesDirectoryExist (dceInputDir opts)
    unless inptDirExist $
      throwError (InputNotDirectory (dceInputDir opts))

    let errs = lefts inpts
    unless (null errs) $
      throwError (ParseErrors $ formatErr `map` errs)

    let mPursVer = fmap fst . listToMaybe . rights $ inpts
    when (isNothing mPursVer) $
      throwError (NoInputs (dceInputDir opts) )
    let pursVer = fromJust mPursVer

    case runWriterT $ dceEval (snd `map` rights inpts) >>= flip dce entryPoints of
      Left err -> throwError (CompilationError err)
      Right (mods, warns) -> do
        relPath <- lift getCurrentDirectory
        lift $ traverse (hPutStrLn stderr . uncurry (displayDCEWarning relPath)) (zip (zip [1..] (repeat (length warns))) warns)
        liftIO $ runCodegen mods (dceInputDir opts) (dceOutputDir opts)
        when (dceDumpCoreFn opts)
          (liftIO $ runDumpCoreFn pursVer mods (dceOutputDir opts))
  where
    runCodegen :: [CoreFn.ModuleT () CoreFn.Ann] -> FilePath -> FilePath -> IO ()
    runCodegen mods inputDir outputDir = do
      -- I need to run `codegen` from `MakeActions` directly
      -- runMake opts (make ...) accepts `PureScript.AST.Declarations.Module`
      (makeErrors, makeWarnings) <- P.runMake (P.Options True False False False) $ runSupplyT 0 (forM mods codegen)
      printWarningsAndErrors False False makeWarnings makeErrors
      where
      codegen :: CoreFn.ModuleT () CoreFn.Ann -> SupplyT P.Make ()
      codegen m@(CoreFn.Module _ mn _ _ _ mf _) = do
        let foreignInclude =
              if null mf
                then Nothing
                else Just $ Imp.App Nothing (Imp.Var Nothing "require") [Imp.StringLiteral Nothing "./foreign"]
        rawJs <- P.moduleToJs m foreignInclude
        let pjs = P.prettyPrintJS rawJs
        let filePath = T.unpack (P.runModuleName mn)
            jsFile = outputDir </> filePath </> "index.js"
            foreignInFile = inputDir </> filePath </> "foreign.js"
            foreignOutFile = outputDir </> filePath </> "foreign.js"
        when (isJust foreignInclude) $ do
          lift $ P.makeIO
            (const (P.ErrorMessage [] $ P.CannotReadFile foreignInFile))
            (createDirectoryIfMissing True (outputDir </> filePath))
          if dceForeign opts
            then do
              jsCode <- lift $ P.makeIO
                (const $ P.ErrorMessage [] $ P.CannotReadFile foreignInFile)
                (B.unpack <$> B.readFile foreignInFile)
              case JS.parse jsCode foreignInFile of
                Right (JS.JSAstProgram ss ann) -> do
                  let ss' = dceForeignModule (fst <$> mf) ss
                      jsAst' = JS.JSAstProgram ss' ann
                  lift $ P.makeIO
                    (const $ P.ErrorMessage [] $ P.CannotWriteFile foreignOutFile)
                    (B.writeFile foreignOutFile (TE.encodeUtf8 . T.toStrict $ JS.renderToText jsAst'))
                Right _ -> throwError (P.errorMessage $ P.ErrorParsingFFIModule foreignInFile (Just P.InvalidTopLevel))
                _ -> throwError (P.errorMessage $ P.ErrorParsingFFIModule foreignInFile Nothing)
            else
              lift $ P.makeIO
                (const (P.ErrorMessage [] (P.CannotReadFile foreignInFile)))
                (copyFile foreignInFile foreignOutFile)
        lift $ writeTextFile jsFile $ TE.encodeUtf8 pjs

    formatErr :: (FilePath, JSONPath, String) -> Text
    formatErr (f, p, err) =
      if dceVerbose opts
        then T.pack $ f ++ ":\n    " ++ A.formatError p err
        else T.pack f

    runDumpCoreFn :: Version -> [CoreFn.ModuleT () CoreFn.Ann] -> FilePath -> IO ()
    runDumpCoreFn pursVer mods outputDir = do
      let jsons = (\m@(CoreFn.Module _ mn _ _ _ _ _ )
            -> ( outputDir </> T.unpack (P.runModuleName mn) </> "corefn.json"
               , A.object [ (P.runModuleName mn, CoreFn.moduleToJSON pursVer m) ]
               )) <$> mods
      sequence_ $ uncurry writeJsonFile <$> jsons

    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

    writeTextFile :: FilePath -> B.ByteString -> P.Make ()
    writeTextFile path text = P.makeIO (const (P.ErrorMessage [] $ P.CannotWriteFile path)) $ do
      mkdirp path
      B.writeFile path text

    writeJsonFile :: FilePath -> Value -> IO ()
    writeJsonFile path v = do
      mkdirp path
      B.writeFile path . TE.encodeUtf8 . T.toStrict . encodeToLazyText $ v

runDCECommand :: DCEOptions -> IO ()
runDCECommand opts = do
  res <- runExceptT (dceCommand opts)
  relPath <- getCurrentDirectory
  case res of
    Left e  -> (hPutStrLn stderr . formatDCEAppError relPath $ e) *> exitFailure
    Right _ -> exitSuccess

command :: Opts.Parser (IO ())
command =  runDCECommand <$> (Opts.helper <*> dceOptions)
