-- | `zephyr` command line option parser
--
module Command.Options
  ( Options (..)
  , parseOptions
  )  where

import           Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable (for)

import qualified Language.PureScript as P
import           Language.PureScript.DCE.Errors (EntryPoint (..))

import qualified Options.Applicative as Opts


-- | @zephyr@ options
--
data Options = Options
  { optEntryPoints       :: [EntryPoint]
  -- ^ List of entry points.
  , optInputDir          :: FilePath
  -- ^ Input directory, default: @outout@.
  , optOutputDir         :: FilePath
  -- ^ Output directory, default: @dce-output@.
  , optVerbose           :: Bool
  -- ^ Verbose output.
  , optForeign           :: Bool
  -- ^ Dead code eliminate foreign javascript module.
  , optPureScriptOptions :: P.Options
  -- ^ PureScription options
  , optUsePrefix         :: Bool
  , optJsonErrors        :: Bool
  -- ^ Print errors in `JSON` format; default 'False'.
  , optEvaluate          :: Bool
  -- ^ Rewirite using an evaluation; it can reduce literal expressions; default
  -- 'False'.
  }


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

dceEvalOpt :: Opts.Parser Bool
dceEvalOpt = Opts.switch $
     Opts.short 'e'
  <> Opts.long "evaluate"
  <> Opts.showDefault
  <> Opts.help "rewrite using simple evaluation"

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

parseOptions :: Opts.Parser Options
parseOptions = Options
  <$> Opts.many entryPointOpt
  <*> inputDirectoryOpt
  <*> outputDirectoryOpt
  <*> verboseOutputOpt
  <*> dceForeignOpt
  <*> pureScriptOptions
  <*> (not <$> noPrefix)
  <*> jsonErrors
  <*> dceEvalOpt

