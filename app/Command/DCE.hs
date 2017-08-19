{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Dead code elimination command based on `Language.PureScript.CoreFn.DCE`.
module Command.DCE
  ( command
  , runDCECommand
  , DCEOptions(..)
  , dceOptions
  , EntryPoint(..)
  , entryPoint
  ) where

import           Control.Monad
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Except
import           Control.Monad.Supply
import qualified Data.Aeson as A
import           Data.Aeson.Internal (JSONPath)
import           Data.Aeson.Internal as A
import           Data.Aeson.Parser (eitherDecodeWith, json)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson.Types (Value)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8 as BU8
import           Data.Bool (bool)
import           Data.Either (Either, lefts, rights)
import           Data.List (init, last, null)
import           Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
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
import qualified Language.PureScript.DCE as P
import qualified Language.PureScript.Errors.JSON as P
import qualified Options.Applicative as Opts
import           System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory)
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitFailure, exitSuccess)
import           System.FilePath ((</>), takeDirectory)
import           System.FilePath.Glob (compile, globDir1)
import           System.IO (stderr)
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

data DCEOptions = DCEOptions
  { dceEntryPoints :: [EntryPoint]
  , dceInputDir :: FilePath
  , dceOutputDir :: FilePath
  , dceDumpCoreFn :: Bool
  , dceVerbose :: Bool
  , dceOptimize :: Int
  }

inputDirectory :: Opts.Parser FilePath
inputDirectory = Opts.strOption $
     Opts.short 'i'
  <> Opts.long "purs-output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The purs output directory"

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "dce-output"
  <> Opts.value "dce-output"
  <> Opts.showDefault
  <> Opts.help "The dce output directory"

newtype EntryPoint = EntryPoint { runEntryPoint :: (P.Qualified P.Ident) }

instance Read EntryPoint where
  readsPrec _ s = case unsnoc (T.splitOn "." (T.pack s)) of
      Just (as, a) | length as > 0 -> [(EntryPoint (P.mkQualified (P.Ident a) (P.ModuleName $ P.ProperName <$> as)), "")]
                   | otherwise     -> [(EntryPoint (P.Qualified Nothing (P.Ident a)), "")]
      Nothing                      -> []
    where
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc [] = Nothing
    unsnoc as = Just (init as, last as)

entryPoint :: Opts.Parser EntryPoint
entryPoint = Opts.argument (Opts.auto >>= checkIfQualified) $
     Opts.metavar "entry-point"
  <> Opts.help "Qualified identifier. All code which is not a transitive dependency of an entry point will be removed. You can pass multiple entry points."
  where
  checkIfQualified (EntryPoint q@(P.Qualified Nothing _)) = fail $
    "not a qualified indentifier: '" ++ T.unpack (P.showQualified P.runIdent q) ++ "'"
  checkIfQualified e = return e

dumpCoreFn :: Opts.Parser Bool
dumpCoreFn = Opts.switch $
     Opts.long "dump-corefn"
  <> Opts.showDefault
  <> Opts.help "Dump the (functional) core representation of the compiled and dce-ed code at dce-output/*/corefn.json"

verboseOutput :: Opts.Parser Bool
verboseOutput = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose"
  <> Opts.showDefault
  <> Opts.help "Verbose parser CoreFn errors."

optimizeLevel :: Opts.Parser Int
optimizeLevel = Opts.option Opts.auto $
     Opts.short 'O'
  <> Opts.value 0
  <> Opts.showDefault
  <> Opts.help "Optimizer level, with -O1 unused exports of foreign modules will be removed (without checking if they are used elsewhere in the foreign module)."

dceOptions :: Opts.Parser DCEOptions
dceOptions = DCEOptions
  <$> Opts.some entryPoint
  <*> inputDirectory
  <*> outputDirectory
  <*> dumpCoreFn
  <*> verboseOutput
  <*> optimizeLevel

readInput :: [FilePath] -> IO [Either (FilePath, JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)]
readInput inputFiles = forM inputFiles (\f -> addPath f . decodeCoreFn <$> B.readFile f)
  where
  decodeCoreFn :: B.ByteString -> Either (JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)
  decodeCoreFn = eitherDecodeWith json (A.iparse CoreFn.moduleFromJSON)

  addPath
    :: FilePath
    -> Either (JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)
    -> Either (FilePath, JSONPath, String) (Version, CoreFn.ModuleT () CoreFn.Ann)
  addPath f = either (Left . incl) Right
    where incl (l,r) = (f,l,r)

data DCEError
  = DCEParseErrors [Text]
  | DCEInputNotDirectory FilePath
  | DCENoInputs FilePath

formatDCEError :: DCEError -> Text
formatDCEError (DCEParseErrors errs)
  = "error: failed parsing:\n  " <> (T.intercalate "\n  " errs)
formatDCEError (DCENoInputs path)
  = "error: inputs found under \"" <> T.pack path <> "\" directory"
formatDCEError (DCEInputNotDirectory path)
  = "error: directory \"" <> T.pack path <> "\" does not exist"

dceCommand :: DCEOptions -> ExceptT DCEError IO ()
dceCommand opts = do
    let entryPoints = runEntryPoint <$> (dceEntryPoints opts)
        cfnGlb = compile "**/corefn.json"
    inpts <- liftIO $ globDir1 cfnGlb (dceInputDir opts) >>= readInput

    inptDirExist <- lift $ doesDirectoryExist (dceInputDir opts)
    when (not inptDirExist) $
      throwError (DCEInputNotDirectory (dceInputDir opts))

    let errs = lefts inpts
    when (not . null $ errs) $
      throwError (DCEParseErrors $ formatErr `map` errs)

    let mPursVer = fmap fst . listToMaybe . rights $ inpts
    when (isNothing mPursVer) $
      throwError (DCENoInputs (dceInputDir opts) )
    let pursVer = fromJust mPursVer

    let mods = P.dce (snd `map` rights inpts) entryPoints
    if dceDumpCoreFn opts
      then liftIO $ runDumpCoreFn pursVer mods (dceOutputDir opts)
      else liftIO $ runCodegen mods (dceInputDir opts) (dceOutputDir opts)

  where
    runCodegen :: [CoreFn.ModuleT () CoreFn.Ann] -> FilePath -> FilePath -> IO ()
    runCodegen mods inputDir outputDir = do
      -- I need to run `codegen` from `MakeActions` directly
      -- runMake opts (make ...) accepts `PureScript.AST.Declarations.Module`
      (makeErrors, makeWarnings) <- P.runMake (P.Options True False False False) $ runSupplyT 0 (forM mods codegen)
      printWarningsAndErrors False False makeWarnings makeErrors
      where
      codegen :: CoreFn.ModuleT () CoreFn.Ann -> SupplyT P.Make ()
      codegen m@(CoreFn.Module _ mn _ _ mf _) = do
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
          if dceOptimize opts >= 1
            then do
              jsCode <- lift $ P.makeIO
                (const $ P.ErrorMessage [] $ P.CannotReadFile foreignInFile)
                (B.unpack <$> B.readFile foreignInFile)
              case JS.parse jsCode foreignInFile of
                Right (JS.JSAstProgram ss ann) -> do
                  let ss' = P.dceForeignModule (fst <$> mf) ss
                      jsAst' = JS.JSAstProgram ss' ann
                  lift $ P.makeIO
                    (const $ P.ErrorMessage [] $ P.CannotWriteFile foreignOutFile)
                    (B.writeFile foreignOutFile (encodeUtf8 $ JS.renderToText jsAst'))
                Right _ -> throwError (P.errorMessage $ P.ErrorParsingFFIModule foreignInFile (Just P.InvalidTopLevel))
                _ -> throwError (P.errorMessage $ P.ErrorParsingFFIModule foreignInFile Nothing)
            else
              lift $ P.makeIO
                (const (P.ErrorMessage [] (P.CannotReadFile foreignInFile)))
                (copyFile foreignInFile foreignOutFile)
        lift $ writeTextFile jsFile (B.fromStrict $ TE.encodeUtf8 pjs)
      
    formatErr :: (FilePath, JSONPath, String) -> Text
    formatErr (f, p, err) = 
      if dceVerbose opts
        then T.pack $ f ++ ":\n    " ++ A.formatError p err
        else T.pack f 

    runDumpCoreFn :: Version -> [CoreFn.ModuleT () CoreFn.Ann] -> FilePath -> IO ()
    runDumpCoreFn pursVer mods outputDir = do
      let jsons = (\m@(CoreFn.Module _ mn _ _ _ _ )
            -> ( outputDir </> T.unpack (P.runModuleName mn) </> "corefn.json"
               , A.object [ (P.runModuleName mn, CoreFn.moduleToJSON pursVer m) ]
               )) <$> mods
      sequence_ $ (uncurry writeJsonFile) <$> jsons

    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

    writeTextFile :: FilePath -> B.ByteString -> P.Make ()
    writeTextFile path text = P.makeIO (const (P.ErrorMessage [] $ P.CannotWriteFile path)) $ do
      mkdirp path
      B.writeFile path text

    writeJsonFile :: FilePath -> Value -> IO ()
    writeJsonFile path v = do
      mkdirp path
      B.writeFile path (encodeUtf8 $ encodeToLazyText v)

runDCECommand :: DCEOptions -> IO ()
runDCECommand opts = do
  res <- runExceptT (dceCommand opts)
  case res of
    Left e  -> (T.hPutStrLn stderr . formatDCEError $ e) *> exitFailure
    Right _ -> exitSuccess

command :: Opts.Parser (IO ())
command =  runDCECommand <$> (Opts.helper <*> dceOptions)
