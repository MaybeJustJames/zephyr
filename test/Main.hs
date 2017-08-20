module Main (main) where

import           Prelude ()
import           Prelude.Compat
import           Control.Monad (join, when)
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.List (last)
import           Data.Foldable (forM_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (sequence)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, setCurrentDirectory)
import           System.Exit (ExitCode(..))
import           System.IO (hSetEncoding, putStrLn, stdout, stderr, utf8)
import           System.Process (readProcessWithExitCode)
import           Test.Hspec
import           Test.Hspec.Core.Spec (SpecM)
import           Test.HUnit (assertEqual)

import qualified TestDCE

coreLibs :: [(Text, Bool, [Text])]
coreLibs =
  [ ("git@github.com:alexmingoia/purescript-pux", True, ["Test.Main.main"])
  , ("git@github.com:bodil/purescript-smolder", False, ["Test.Main.main"])
  , ("git@github.com:bodil/purescript-signal", False, ["Test.Main.main"])
  , ("git@github.com:bodil/purescript-test-unit", False, ["Test.Main.main"])
  , ("git@github.com:pag31/purescript-behaviours", False, ["Test.Main.main"])
  , ("git@github.com:pag31/purescript-thermite", False, ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-aff", False, ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-bigints", False, ["Test.Main.main"])
  -- |
  -- requires window object
  -- , ("git@github.com:slamdata/purescript-halogen-vdom", False, ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-matryoshka", False, ["Test.Main.main"])
  -- | does not compile
  -- , ("git@github.com:slamdata/purescript-mockfree", False, ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-routing", False, ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-search", False, ["Test.Main.main"])
  -- | does not compile
  -- , ("git@github.com:slamdata/purescript-stalling-coroutines", False, ["Test.Main.main"])
  -- | does not compile
  -- , ("git@github.com:slamdata/purescript-xpath", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-codecs", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-core", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-generic", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-traversals", False, ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-drawing", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-foreign-lens", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-handlebars", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-js-date", False, ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-jquery", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-lens", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-profunctor-lenses", False, ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/pulp", True, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-nullable", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-options", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-parsing", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-precise", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-string-parsers", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-strongcheck", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-unicode", False, ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-arraybuffer-types", False, ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-freet", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-js-timers", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-unsafe-reference", False, ["Test.Main.main"])
  -- | test suit does runs infinite loop
  -- , ("git@github.com:purescript-contrib/purescript-aff-coroutines", False, ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-coroutines", False, ["Test.Main.main"])
  ]

data TestError
  = GitError Text ExitCode String
  | NpmError Text ExitCode String
  | BowerError Text ExitCode String
  | PursError Text ExitCode String
  | ZephyrError Text ExitCode String
  | NodeError Text ExitCode String String
  deriving (Eq)

instance Show TestError where
  show (GitError repo ec err)
    = "git failed      \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (NpmError repo ec err)
    = "npm failed      \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (BowerError repo ec err)
    = "bower failed    \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (PursError repo ec err)
    = "purs failed     \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (ZephyrError repo ec err)
    = "zephyr failed   \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (NodeError repo ec std err)
    = "node failed     \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n\n" ++ std ++ "\n\n" ++ err

isGitError :: TestError -> Bool
isGitError (GitError _ _ _) = True
isGitError _ = False

runTestLib :: (Text, Bool, [Text]) -> ExceptT TestError IO ()
runTestLib (repo, nodeInstall, entryPoints) = do
  let dir = last $ T.splitOn "/" repo

  repoExist <- lift $ doesDirectoryExist $ T.unpack dir
  when (not repoExist) $ do
    (ecGit, _, errGc) <- lift $ readProcessWithExitCode "git" ["clone", "--depth", "1", T.unpack repo, T.unpack dir] ""
    when (ecGit /= ExitSuccess) (throwError (GitError repo ecGit errGc))
  lift $ setCurrentDirectory (T.unpack dir)

  nodeModulesExists <- lift $ doesDirectoryExist "node_modules"
  when (nodeInstall && not nodeModulesExists) $ do
    (ecNpm, _, errNpm) <- lift $ readProcessWithExitCode "npm" ["install"] ""
    when (ecNpm /= ExitSuccess) (throwError (NpmError repo ecNpm errNpm))
      
  bowerComponentsExists <- lift $ doesDirectoryExist "bower_components"
  when (not bowerComponentsExists) $ do
    (ecBower, _, errBower) <- lift $ readProcessWithExitCode "bower" ["install"] ""
    when (ecBower /= ecBower) (throwError (BowerError repo ecBower errBower))

  outputDirExists <- lift $ doesDirectoryExist "output"
  when (not outputDirExists) $ do
    (ecPurs, _, errPurs) <- lift $ readProcessWithExitCode "purs" ["compile", "--dump-corefn", "bower_components/purescript-*/src/**/*.purs", "src/**/*.purs", "test/**/*.purs"] ""
    when (ecPurs /= ExitSuccess) (throwError $ PursError repo ecPurs errPurs)

  outputDirExists <- lift $ doesDirectoryExist "dce-output"
  when outputDirExists $
    lift $ removeDirectoryRecursive "dce-output"
  (ecZephyr, _, errZephyr) <- lift $ readProcessWithExitCode "zephyr" (["-O", "1"] ++ T.unpack `map` entryPoints) ""
  when (ecZephyr /= ExitSuccess) (throwError $ ZephyrError repo ecZephyr errZephyr)

  (ecNode, stdNode , errNode) <- lift $ readProcessWithExitCode "node" ["-e", "require('./dce-output/Test.Main/index.js').main()"] ""
  when (ecNode /= ExitSuccess) (throwError $ NodeError repo ecNode stdNode errNode)

assertRuns
  :: (Text, Bool, [Text])
  -> Expectation
assertRuns l = do
  res <- runExceptT . runTestLib $ l
  when (either isGitError (const False) res)
    (setCurrentDirectory "..")
  assertEqual "should run" (Right ()) res

contribSpec :: Spec
contribSpec = do
  context "test libraries" $ 
    forM_ coreLibs $ \l@(repo, _, _) ->
        specify (T.unpack repo) $ assertRuns l
  where

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestDCE.main

  createDirectoryIfMissing False ".temp"
  setCurrentDirectory ".temp"

  hspec contribSpec
