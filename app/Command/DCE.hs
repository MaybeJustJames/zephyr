-- | Dead code elimination command based on `Language.PureScript.CoreFn.DCE`.
module Command.DCE
  ( runDCECommand
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
import qualified Data.Aeson.Internal as A
import           Data.Aeson.Parser (eitherDecodeWith, json)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL.Char8 (unpack)
import qualified Data.ByteString.Lazy.UTF8 as BU8
import           Data.Bool (bool)
import           Data.Either (Either, lefts, rights, partitionEithers)
import           Data.Foldable (for_, traverse_)
import           Data.List (intercalate, null)
import qualified Data.Map as M
import           Data.Maybe (isNothing, listToMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import           Data.Traversable (for)
import           Data.Version (Version)
import           Formatting (sformat, string, stext, (%))
import qualified Language.PureScript.Docs.Types as Docs
import qualified Language.JavaScript.Parser as JS
import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
import           Language.PureScript.DCE
import qualified Language.PureScript.Errors.JSON as P
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import           System.Directory (doesDirectoryExist, getCurrentDirectory)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>))
import           System.FilePath.Glob (compile, globDir1)
import           System.IO (hPutStrLn, stderr)

import           Command.DCEOptions
import           Language.PureScript.DCE.Errors (EntryPoint (..))

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
  <> Opts.help "Qualified identifier or a module name (it may be prefixed with `ident:` or `module:`). All code which is not a transitive dependency of an entry point (or any exported identifier from a give module) will be removed. You can pass multiple entry points."
  where
  checkIfQualified (EntryPoint q@(P.Qualified Nothing _)) = fail $
    "not a qualified indentifier: '" ++ T.unpack (P.showQualified P.runIdent q) ++ "'"
  checkIfQualified e = return e

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

comments :: Opts.Parser Bool
comments = Opts.switch $
     Opts.short 'c'
  <> Opts.long "comments"
  <> Opts.help "Include comments in the generated code"

verboseErrors :: Opts.Parser Bool
verboseErrors = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose-errors"
  <> Opts.help "Display verbose error messages"

codegenTargets :: Opts.Parser [P.CodegenTarget]
codegenTargets = Opts.option targetParser $
     Opts.short 'g'
  <> Opts.long "codegen"
  <> Opts.value [P.JS]
  <> Opts.help
      ( "Specifies comma-separated codegen targets to include. "
      <> targetsMessage
      <> " The default target is 'js', but if this option is used only the targets specified will be used."
      )

dceNoEvalOpt :: Opts.Parser Bool
dceNoEvalOpt = Opts.flag True False $
     Opts.short 'e'
  <> Opts.long "no-eval"
  <> Opts.showDefault
  <> Opts.internal
  <> Opts.help "do not evaluate"

targets :: M.Map String P.CodegenTarget
targets = M.fromList
  [ ("js", P.JS)
  , ("sourcemaps", P.JSSourceMap)
  , ("corefn", P.CoreFn)
  ]

targetsMessage :: String
targetsMessage = "Accepted codegen targets are '" <> intercalate "', '" (M.keys targets) <> "'."

targetParser :: Opts.ReadM [P.CodegenTarget]
targetParser =
  Opts.str >>= \s ->
    for (T.split (== ',') s)
      $ maybe (Opts.readerError targetsMessage) pure
      . flip M.lookup targets
      . T.unpack
      . T.strip

noPrefix :: Opts.Parser Bool
noPrefix = Opts.switch $
     Opts.short 'p'
  <> Opts.long "no-prefix"
  <> Opts.help "Do not include comment header"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
     Opts.long "json-errors"
  <> Opts.help "Print errors to stderr as JSON"

pureScriptOptions :: Opts.Parser P.Options
pureScriptOptions =
  P.Options
    <$> verboseErrors
    <*> (not <$> comments)
    <*> (handleTargets <$> codegenTargets)
  where
    -- Ensure that the JS target is included if sourcemaps are
    handleTargets :: [P.CodegenTarget] -> S.Set P.CodegenTarget
    handleTargets ts = S.fromList (if P.JSSourceMap `elem` ts then P.JS : ts else ts)

dceOptions :: Opts.Parser DCEOptions
dceOptions = DCEOptions
  <$> Opts.many entryPointOpt
  <*> inputDirectoryOpt
  <*> outputDirectoryOpt
  <*> verboseOutputOpt
  <*> dceForeignOpt
  <*> pureScriptOptions
  <*> (not <$> noPrefix)
  <*> jsonErrors
  <*> dceNoEvalOpt

readInput :: [FilePath] -> IO [Either (FilePath, JSONPath, String) (Version, CoreFn.Module CoreFn.Ann)]
readInput inputFiles = forM inputFiles (\f -> addPath f . decodeCoreFn <$> BSL.readFile f)
  where
  decodeCoreFn :: BSL.ByteString -> Either (JSONPath, String) (Version, CoreFn.Module CoreFn.Ann)
  decodeCoreFn = eitherDecodeWith json (A.iparse CoreFn.moduleFromJSON)

  addPath
    :: FilePath
    -> Either (JSONPath, String) (Version, CoreFn.Module CoreFn.Ann)
    -> Either (FilePath, JSONPath, String) (Version, CoreFn.Module CoreFn.Ann)
  addPath f = either (Left . incl) Right
    where incl (l,r) = (f,l,r)
--
-- | Argumnets: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . BU8.toString . A.encode $
    P.JSONResult (P.toJSONErrors verbose P.Warning warnings)
               (either (P.toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

data DCEAppError
  = ParseErrors [Text]
  | InputNotDirectory FilePath
  | NoInputs FilePath
  | CompilationError (DCEError 'Error)

formatDCEAppError :: DCEOptions -> FilePath -> DCEAppError -> Text
formatDCEAppError opts _ (ParseErrors errs) =
  let errs' =
        if dceVerbose opts
        then errs
        else take 5 errs ++ case length $ drop 5 errs of
          0 -> []
          x -> ["... (" <> T.pack (show x) <> " more)"]
  in sformat
        (string%": Failed parsing:\n  "%stext)
        (colorString errorColor "Error")
        (T.intercalate "\n\t" errs')
formatDCEAppError _ _ (NoInputs path)
  = sformat
        (stext%": No inputs found under "%string%" directory.\n"
              %"       Please run `purs compile --codegen corefn ..` or"
              %"`pulp build -- --codegen corefn`")
        (colorText errorColor "Error")
        (colorString codeColor path)
formatDCEAppError _ _ (InputNotDirectory path)
  = sformat
        (stext%": Directory "%string%" does not exists.")
        (colorText errorColor "Error")
        (colorString codeColor path)
formatDCEAppError _ relPath (CompilationError err)
  = T.pack $ displayDCEError relPath err


getEntryPoints
  :: [CoreFn.Module CoreFn.Ann]
  -> [EntryPoint]
  -> [Either EntryPoint (P.Qualified P.Ident)]
getEntryPoints mods = go []
  where
  go acc [] = acc
  go acc ((EntryPoint i) : eps)  = 
    if i `fnd` mods
      then go (Right i : acc) eps
      else go (Left (EntryPoint i)  : acc) eps
  go acc ((EntryModule mn) : eps) = go (modExports mn mods ++ acc) eps
  go acc ((err@EntryParseError{}) : eps) = go (Left err : acc) eps

  modExports :: P.ModuleName -> [CoreFn.Module CoreFn.Ann] -> [Either EntryPoint (P.Qualified P.Ident)]
  modExports mn [] = [Left (EntryModule mn)]
  modExports mn (CoreFn.Module{moduleName,moduleExports} : ms)
    | mn == moduleName
    = (Right . flip P.mkQualified mn) `map` moduleExports
    | otherwise
    = modExports mn ms

  fnd :: P.Qualified P.Ident -> [CoreFn.Module CoreFn.Ann] -> Bool
  fnd _ [] = False
  fnd qi@(P.Qualified (Just mn) i) (CoreFn.Module{moduleName,moduleExports} : ms)
    = if moduleName == mn && i `elem` moduleExports
        then True
        else fnd qi ms
  fnd _ _ = False

dceCommand :: DCEOptions -> ExceptT DCEAppError IO ()
dceCommand DCEOptions {..} = do
    -- initial checks
    inptDirExist <- lift $ doesDirectoryExist dceInputDir
    unless inptDirExist $
      throwError (InputNotDirectory dceInputDir)

    -- read files, parse errors
    let cfnGlb = compile "**/corefn.json"
    inpts <- liftIO $ globDir1 cfnGlb dceInputDir >>= readInput
    let errs = lefts inpts
    unless (null errs) $
      throwError (ParseErrors $ formatError `map` errs)

    let mPursVer = fmap fst . listToMaybe . rights $ inpts
    when (isNothing mPursVer) $
      throwError (NoInputs dceInputDir)

    let (notFound, entryPoints) = partitionEithers $ getEntryPoints (fmap snd . rights $ inpts) dceEntryPoints

    when (not $ null notFound) $
      case filter isEntryParseError notFound of
        []   -> throwError (CompilationError $ EntryPointsNotFound notFound)
        perrs ->
          let fn (EntryParseError s) acc = s : acc
              fn _                   acc = acc
          in throwError (CompilationError $ EntryPointsNotParsed (foldr fn [] perrs))

    when (null $ entryPoints) $
      throwError (CompilationError $ NoEntryPoint)

    -- run `dceEval` and `dce` on the `CoreFn`
    (mods, warns) <- mapExceptT (fmap $ first CompilationError)
        $ runWriterT
        $ if dceNoEval
            then return $ dce (snd `map` rights inpts) entryPoints
            else flip dce entryPoints <$> dceEval (snd `map` rights inpts)


    relPath <- liftIO getCurrentDirectory
    liftIO $ traverse_ (hPutStrLn stderr . uncurry (displayDCEWarning relPath)) (zip (zip [1..] (repeat (length warns))) warns)
    let filePathMap = M.fromList $ map (\m -> (CoreFn.moduleName m, Right $ CoreFn.modulePath m)) mods
    foreigns <- P.inferForeignModules filePathMap
    let makeActions = (P.buildMakeActions dceOutputDir filePathMap foreigns dceUsePrefix)
          -- run `dceForeign` in `ffiCodeGen`
          { P.ffiCodegen = \CoreFn.Module{moduleName,moduleForeign} -> liftIO $
                case moduleName `M.lookup` foreigns of
                  Nothing -> return ()
                  Just fp -> do
                    jsCode <- BSL.Char8.unpack <$> BSL.readFile fp
                    case JS.parse jsCode fp of
                      Left _ -> return ()
                      Right (JS.JSAstProgram ss ann) ->
                        let ss'    = dceForeignModule moduleForeign ss
                            jsAst' = JS.JSAstProgram ss' ann
                            foreignFile
                                  = dceOutputDir </> T.unpack (P.runModuleName moduleName) </> "foreign.js"
                        in
                          BSL.writeFile foreignFile (TE.encodeUtf8 $ JS.renderToText jsAst')
                      Right _ -> return ()
          }
    (makeErrors, makeWarnings) <-
        liftIO
        $ P.runMake dcePureScriptOptions
        $ runSupplyT 0 $ traverse (\m -> P.codegen makeActions m (Docs.Module (CoreFn.moduleName m) Nothing [] []) mempty) mods
    when dceForeign $
      traverse_ (liftIO . P.runMake dcePureScriptOptions . P.ffiCodegen makeActions) mods

    -- copy extern files
    -- we do not have access to data to regenerate extern files (they relay on
    -- more information than is present in `CoreFn.Module`).
    for_ mods $ \m -> lift $ do
      let mn = P.runModuleName $ CoreFn.moduleName m
      exts <- BSL.readFile (dceInputDir </> T.unpack mn </> "externs.json")
      BSL.writeFile (dceOutputDir </> T.unpack mn </> "externs.json") exts
    liftIO $ printWarningsAndErrors (P.optionsVerboseErrors dcePureScriptOptions) dceJsonErrors
        (suppressFFIErrors makeWarnings)
        (either (Left . suppressFFIErrors) Right makeErrors)
    return ()
  where
    formatError :: (FilePath, JSONPath, String) -> Text
    formatError (f, p, err) =
      if dceVerbose
        then sformat (string%":\n    "%string) f (A.formatError p err)
        else T.pack f

    -- a hack: purescript codegen function reads FFI from disk, and checks
    -- against it
    suppressFFIErrors :: P.MultipleErrors -> P.MultipleErrors
    suppressFFIErrors (P.MultipleErrors errs) = P.MultipleErrors $ filter fn errs
      where
      fn (P.ErrorMessage _ P.UnnecessaryFFIModule{})     = False
      fn (P.ErrorMessage _ P.UnusedFFIImplementations{}) = False
      fn _                                               = True

runDCECommand
  :: DCEOptions
  -> IO ()
runDCECommand opts = do
  res <- runExceptT $ dceCommand opts
  relPath <- getCurrentDirectory
  case res of
    Left e  ->
         (hPutStrLn stderr . T.unpack . formatDCEAppError opts relPath $ e)
      *> exitFailure
    Right{} ->
      exitSuccess
