{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Dead code elimination command based on `Language.PureScript.CoreFn.DCE`.
--
module Command.Run
  ( runZephyr
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Supply
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except
import           Control.Exception
import           Control.Concurrent.QSem
import qualified Control.Concurrent.Async as Async

import qualified Data.Aeson as A
import           Data.Aeson.Internal (JSONPath)
import qualified Data.Aeson.Internal as A
import           Data.Aeson.Parser (eitherDecodeWith, json)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL.Char8 (unpack)
import qualified Data.ByteString.Lazy.UTF8 as BU8
import           Data.Bool (bool)
import           Data.Either (lefts, rights, partitionEithers)
import           Data.Foldable (for_, traverse_)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe (isNothing, listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import           Data.Version (Version)
import           Formatting (sformat, string, stext, (%))

import           GHC.Conc.Sync (getNumCapabilities)

import qualified Language.PureScript.Docs.Types as Docs
import qualified Language.JavaScript.Parser as JS
import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.CoreFn.FromJSON as CoreFn
import qualified Language.PureScript.Errors.JSON as P
import qualified System.Console.ANSI as ANSI
import           System.Directory (copyFile, doesDirectoryExist, doesFileExist,
                                   getCurrentDirectory, removeFile)
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>), (-<.>))
import           System.FilePath.Glob (compile, globDir1)
import           System.IO (hPutStrLn, stderr)

import           Command.Options
import           Language.PureScript.DCE.Errors (EntryPoint (..))

import           Language.PureScript.DCE ( DCEError (..)
                                         , Level (..)
                                         )
import qualified Language.PureScript.DCE as DCE


readInput :: [FilePath]
          -> IO [Either
                  (FilePath, JSONPath, String)
                  (Version, CoreFn.Module CoreFn.Ann)
                ]

readInput inputFiles = do
    -- limit parallelizm to at most the number of capablities
    sem <- getNumCapabilities >>= newQSem
    threads <-
      forM inputFiles $ \f -> do
        waitQSem sem
        mask $ \unmask -> Async.async $
          (unmask $ do
            c <- BSL.readFile f
            -- being strict here forces reading the file and promptly closing its file
            -- descriptor
            case decodeCoreFn c of
              Left  (p, e)     -> pure $ Left  (f, p, e)
              Right r@(!_, !_) -> pure $ Right r
          )
          `finally`
            signalQSem sem
    forM threads Async.wait
  where
    decodeCoreFn :: BSL.ByteString
                 -> Either (JSONPath, String)
                           (Version, CoreFn.Module CoreFn.Ann)
    decodeCoreFn = eitherDecodeWith json (A.iparse CoreFn.moduleFromJSON)


-- | Argumnets: verbose, use JSON, warnings, errors
--
printWarningsAndErrors
    :: Bool                      -- ^ be verbose
    -> Bool                      -- ^ use 'JSON'
    -> P.MultipleErrors          -- ^ warnings
    -> Either P.MultipleErrors a -- ^ errors
    -> IO ()

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


-- | Application exception
--
data DCEAppError
  = ParseErrors       ![Text]
  -- ^ parser errors
  | InputNotDirectory !FilePath
  -- ^ input directory does not exists (or is not a directory)
  | NoInputs          !FilePath
  -- ^ no input files
  | DCEAppError  !(DCEError 'Error)
  -- ^ PureScript errors


-- | Render 'DCEAppError' as 'Text'
--
formatDCEAppError :: Options -> FilePath -> DCEAppError -> Text
formatDCEAppError opts _ (ParseErrors errs) =
  let errs' =
        if optVerbose opts
        then errs
        else take 5 errs ++ case length $ drop 5 errs of
          0 -> []
          x -> ["... (" <> T.pack (show x) <> " more)"]
  in sformat
        (string%": Failed parsing:\n  "%stext)
        (DCE.colorString DCE.errorColor "Error")
        (T.intercalate "\n\t" errs')
formatDCEAppError _ _ (NoInputs path)
  = sformat
        (stext%": No inputs found under "%string%" directory.\n"
              %"       Please run `purs compile --codegen corefn ..` or"
              %"`pulp build -- --codegen corefn`")
        (DCE.colorText DCE.errorColor "Error")
        (DCE.colorString DCE.codeColor path)
formatDCEAppError _ _ (InputNotDirectory path)
  = sformat
        (stext%": Directory "%string%" does not exists.")
        (DCE.colorText DCE.errorColor "Error")
        (DCE.colorString DCE.codeColor path)
formatDCEAppError _ relPath (DCEAppError err)
  = T.pack $ DCE.displayDCEError relPath err


-- | Given list of modules and list of entry points, find qualilfied names of
-- roots.
--
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
    modExports mn (CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleExports, CoreFn.moduleReExports } : ms)
      | mn == moduleName
      = (Right . flip P.mkQualified mn) `map` (moduleExports ++ (snd =<< M.toList moduleReExports))
      | otherwise
      = modExports mn ms

    fnd :: P.Qualified P.Ident -> [CoreFn.Module CoreFn.Ann] -> Bool
    fnd _ [] = False
    fnd qi@(P.Qualified (Just mn) i) (pModule@CoreFn.Module{ CoreFn.moduleName } : ms)
      = if moduleName == mn && i `elem` (allExports pModule)
          then True
          else fnd qi ms
    fnd _ _ = False

    allExports :: CoreFn.Module CoreFn.Ann -> [P.Ident]
    allExports CoreFn.Module{ CoreFn.moduleExports, CoreFn.moduleReExports } =
      moduleExports ++ (M.toList moduleReExports >>= snd)


dceCommand :: Options -> ExceptT DCEAppError IO ()
dceCommand Options { optEntryPoints
                   , optInputDir
                   , optOutputDir
                   , optVerbose
                   , optForeign
                   , optPureScriptOptions
                   , optUsePrefix
                   , optJsonErrors
                   , optEvaluate
                   } = do
    -- initial checks
    inptDirExist <- lift $ doesDirectoryExist optInputDir
    unless inptDirExist $
      throwError (InputNotDirectory optInputDir)

    -- read files, parse errors
    let cfnGlb = compile "**/corefn.json"
    inpts0 <- liftIO $ globDir1 cfnGlb optInputDir >>= readInput
    -- force inputs sequentially
    inpts  <- liftIO $ traverse evaluate inpts0
    let errs = lefts inpts
    unless (null errs) $
      throwError (ParseErrors $ formatError `map` errs)

    let mPursVer = fmap fst . listToMaybe . rights $ inpts
    when (isNothing mPursVer) $
      throwError (NoInputs optInputDir)

    let (notFound, entryPoints) =
          partitionEithers
            (getEntryPoints
              (fmap snd . rights $ inpts)
              optEntryPoints)

    when (not $ null notFound) $
      case filter DCE.isEntryParseError notFound of
        []   -> throwError (DCEAppError $ EntryPointsNotFound notFound)
        perrs ->
          let fn (EntryParseError s) acc = s : acc
              fn _                   acc = acc
          in throwError (DCEAppError $ EntryPointsNotParsed (foldr fn [] perrs))

    when (null entryPoints) $
      throwError (DCEAppError NoEntryPoint)

    -- run `evaluate` and `runDeadCodeElimination` on `CoreFn` representation
    let mods = if optEvaluate
                  then DCE.runDeadCodeElimination
                        entryPoints
                        (DCE.evaluate (snd `map` rights inpts))
                  else DCE.runDeadCodeElimination
                        entryPoints
                        (snd `map` rights inpts)

    let filePathMap =
          M.fromList
            (map
              (\m -> (CoreFn.moduleName m, Right $ CoreFn.modulePath m))
              mods)
    foreigns <- P.inferForeignModules filePathMap
    let makeActions = (P.buildMakeActions optOutputDir filePathMap foreigns optUsePrefix)
          { P.ffiCodegen = \CoreFn.Module{ CoreFn.moduleName, CoreFn.moduleForeign } -> liftIO $ do
                let codegenTargets = P.optionsCodegenTargets optPureScriptOptions
                when (S.member P.JS codegenTargets) $ do
                  case moduleName `M.lookup` foreigns of
                    -- run `runForeignModuleDeadCodeElimination`
                    Just path | optForeign  -> do
                      jsCode <- BSL.Char8.unpack <$> BSL.readFile path
                      case JS.parseModule jsCode path of
                        Left _ -> return ()
                        Right (JS.JSAstModule items ann) ->
                          let items'    = DCE.runForeignModuleDeadCodeElimination moduleForeign items
                              jsAst' = JS.JSAstModule items' ann
                              foreignFile = optOutputDir
                                        </> T.unpack (P.runModuleName moduleName)
                                        </> "foreign.js"
                          in BSL.writeFile foreignFile (TE.encodeUtf8 $ JS.renderToText jsAst')
                        Right _ -> return ()

                    Just path -> do
                      let filePath = T.unpack (P.runModuleName moduleName)
                          outputPath = optInputDir </> filePath </> "foreign.js"
                      -- prefer foreign module in 'optOutputDir'.
                      bool path outputPath <$> doesFileExist outputPath
                        >>= (`copyFile` (optOutputDir </> filePath </> "foreign.js"))

                    Nothing -> pure ()
            }

    (makeErrors, makeWarnings) <-
        liftIO
        $ P.runMake optPureScriptOptions
        $ runSupplyT 0
        $ traverse
            (\m ->
              P.codegen makeActions m
                        (Docs.Module (CoreFn.moduleName m) Nothing [] [])
                        (moduleToExternsFile m))
            mods

    traverse_ (liftIO . P.runMake optPureScriptOptions . P.ffiCodegen makeActions) mods

    -- copy extern files; We do not have access to data to regenerate extern
    -- files (they relay on more information than is present in 'CoreFn.Module'
    -- represenation).
    for_ mods $ \m -> lift $ do
      let mn = CoreFn.moduleName m
      copyExterns mn "cbor" <|> do
        -- zephyr will always generate "externs.cbor" file, if we are working
        -- on a project using purescript-0.13.6 we need to remove it.
        removeFile (optOutputDir </> (T.unpack $ P.runModuleName mn) </> "externs.cbor")
        copyExterns mn "json"
    liftIO $
      printWarningsAndErrors
        (P.optionsVerboseErrors optPureScriptOptions)
        optJsonErrors
        (suppressFFIErrors makeWarnings)
        (either (Left . suppressFFIErrors) Right makeErrors)

  where

    formatError :: (FilePath, JSONPath, String) -> Text
    formatError (f, p, err) =
      if optVerbose
        then sformat (string%":\n    "%string) f (A.formatError p err)
        else T.pack f

    copyExterns :: P.ModuleName -> String -> IO ()
    copyExterns mn extension = do
      let filePath = T.unpack . P.runModuleName $ mn
      copyFile (optInputDir  </> filePath </> "externs" -<.> extension)
               (optOutputDir </> filePath </> "externs" -<.> extension)

    -- a hack: purescript codegen function reads FFI from disk, and checks
    -- against it.
    suppressFFIErrors :: P.MultipleErrors -> P.MultipleErrors
    suppressFFIErrors (P.MultipleErrors errs) = P.MultipleErrors $ filter fn errs
      where
        fn (P.ErrorMessage _ P.UnnecessaryFFIModule{})     = False
        fn (P.ErrorMessage _ P.UnusedFFIImplementations{}) = False
        fn _                                               = True

    moduleToExternsFile :: CoreFn.Module a -> P.ExternsFile
    moduleToExternsFile CoreFn.Module {CoreFn.moduleName} = P.ExternsFile {
        P.efVersion      = mempty,
        P.efModuleName   = moduleName,
        P.efExports      = [],
        P.efImports      = [],
        P.efFixities     = [],
        P.efTypeFixities = [],
        P.efDeclarations = [],
        P.efSourceSpan   = P.SourceSpan "none" (P.SourcePos 0 0) (P.SourcePos 0 0)
      }


runZephyr
  :: Options
  -> IO ()
runZephyr opts = do
  res <- runExceptT $ dceCommand opts
  relPath <- getCurrentDirectory
  case res of
    Left e  ->
         (hPutStrLn stderr . T.unpack . formatDCEAppError opts relPath $ e)
      *> exitFailure
    Right{} ->
      exitSuccess
