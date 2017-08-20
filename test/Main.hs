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
import           System.Directory
                  ( createDirectoryIfMissing
                  , doesDirectoryExist
                  , doesFileExist
                  , removeDirectoryRecursive
                  , setCurrentDirectory
                  )
import           System.Exit (ExitCode(..))
import           System.IO (hSetEncoding, putStrLn, stdout, stderr, utf8)
import           System.Process (readProcessWithExitCode)
import           Test.Hspec
import           Test.Hspec.Core.Spec (SpecM)
import           Test.HUnit (assertEqual)

import qualified TestDCE

coreLibs :: [(Text, [Text], [Text])]
coreLibs =
  [ ("git@github.com:alexmingoia/purescript-pux", [], ["Test.Main.main"])
  -- | smolder
  -- test suit does not exit
  , ("git@github.com:bodil/purescript-smolder", [], ["Test.Main.main"])
  , ("git@github.com:bodil/purescript-signal", [], ["Test.Main.main"])
  , ("git@github.com:bodil/purescript-test-unit", [], ["Test.Main.main"])
  -- |
  -- requires window
  -- , ("git@github.com:paf31/purescript-behaviors", [], ["Test.Main.main"])
  -- | thermite
  -- requires to manually install react & react-dom and requires
  -- window objct
  -- , ("git@github.com:paf31/purescript-thermite", [], ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-aff", [], ["Test.Main.main"])
  -- | bigints does not compile
  -- , ("git@github.com:slamdata/purescript-bigints", [], ["Test.Main.main"])
  -- | halogen-dom
  -- requires window object
  -- , ("git@github.com:slamdata/purescript-halogen-vdom", [], ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-matryoshka", [], ["Test.Main.main"])
  -- | does not compile
  -- , ("git@github.com:slamdata/purescript-mockfree", [], ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-routing", [], ["Test.Main.main"])
  , ("git@github.com:slamdata/purescript-search", [], ["Test.Main.main"])
  -- | does not compile
  -- , ("git@github.com:slamdata/purescript-stalling-coroutines", [], ["Test.Main.main"])
  -- | does not compile
  -- , ("git@github.com:slamdata/purescript-xpath", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-codecs", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-core", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-generic", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-argonaut-traversals", [], ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-drawing", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-foreign-lens", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-handlebars", ["handlebars"], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-js-date", [], ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-jquery", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-lens", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-profunctor-lenses", [], ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/pulp", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-nullable", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-options", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-parsing", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-precise", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-string-parsers", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-strongcheck", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-unicode", [], ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-arraybuffer-types", [], ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-freet", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-js-timers", [], ["Test.Main.main"])
  , ("git@github.com:purescript-contrib/purescript-unsafe-reference", [], ["Test.Main.main"])
  -- | couroutine-aff and coroutine test suits run an infinite loop
  -- , ("git@github.com:purescript-contrib/purescript-aff-coroutines", [], ["Test.Main.main"])
  -- , ("git@github.com:purescript-contrib/purescript-coroutines", [], ["Test.Main.main"])
  ]

karmaLibs = 
  [ ("git@github.com:coot/purescript-react-hocs", "Test.Main.main", "Test.Main")
  , ("git@github.com:coot/purescript-react-redox","Test.Karma.Main.main", "Test.Karma.Main")
  ]

data TestError
  = GitError Text ExitCode String
  | NpmError Text ExitCode String
  | BowerError Text ExitCode String
  | PursError Text ExitCode String
  | PursBundleError Text ExitCode String
  | BrowserifyError Text ExitCode String
  | ZephyrError Text ExitCode String
  | NodeError Text ExitCode String String
  deriving (Eq)

instance Show TestError where
  show (GitError repo ec err)
    = "git failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (NpmError repo ec err)
    = "npm failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (BowerError repo ec err)
    = "bower failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (PursError repo ec err)
    = "purs compile failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (PursBundleError repo ec err)
    = "purs bundle failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (BrowserifyError repo ec err)
    = "browserify failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (ZephyrError repo ec err)
    = "zephyr failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n" ++ err
  show (NodeError repo ec std err)
    = "node failed \"" ++ T.unpack repo ++ "\" (" ++ show ec ++ ")\n\n" ++ std ++ "\n\n" ++ err

isGitError :: TestError -> Bool
isGitError (GitError _ _ _) = True
isGitError _ = False

cloneRepo
  :: Text
  -> ExceptT TestError IO ()
cloneRepo repo = do
  let dir = last $ T.splitOn "/" repo

  repoExist <- lift $ doesDirectoryExist $ T.unpack dir
  when (not repoExist) $ do
    (ecGit, _, errGc) <- lift $ readProcessWithExitCode "git" ["clone", "--depth", "1", T.unpack repo, T.unpack dir] ""
    when (ecGit /= ExitSuccess) (throwError (GitError repo ecGit errGc))
  lift $ setCurrentDirectory (T.unpack dir)

npmInstall
  :: Text
  -> [Text]
  -> ExceptT TestError IO ()
npmInstall repo npmModules = do
  npmInstall <- lift $ doesFileExist "package.json"
  nodeModulesExists <- lift $ doesDirectoryExist "node_modules"
  when ((npmInstall || not (null npmModules)) && not nodeModulesExists) $ do
    when (not $ null $ npmModules) $ do
      (ecNpm, _, errNpm) <- lift $ readProcessWithExitCode "npm" (["install"] ++ T.unpack `map` npmModules) ""
      when (ecNpm /= ExitSuccess) (throwError (NpmError repo ecNpm errNpm))
    (ecNpm, _, errNpm) <- lift $ readProcessWithExitCode "npm" ["install"] ""
    when (ecNpm /= ExitSuccess) (throwError (NpmError repo ecNpm errNpm))

bowerInstall
  :: Text
  -> ExceptT TestError IO ()
bowerInstall repo = do
  bowerComponentsExists <- lift $ doesDirectoryExist "bower_components"
  when (not bowerComponentsExists) $ do
    (ecBower, _, errBower) <- lift $ readProcessWithExitCode "bower" ["install"] ""
    when (ecBower /= ecBower) (throwError (BowerError repo ecBower errBower))

pursCompile
  :: Text
  -> ExceptT TestError IO ()
pursCompile repo = do
  outputDirExists <- lift $ doesDirectoryExist "output"
  when (not outputDirExists) $ do
    (ecPurs, _, errPurs) <- lift
      $ readProcessWithExitCode
          "purs"
          [ "compile"
          , "--dump-corefn"
          , "bower_components/purescript-*/src/**/*.purs"
          , "src/**/*.purs"
          , "test/**/*.purs"
          ]
          ""
    when (ecPurs /= ExitSuccess) (throwError $ PursError repo ecPurs errPurs)

runZephyr
  :: Text
  -> [Text]
  -> ExceptT TestError IO ()
runZephyr repo entryPoints = do
  outputDirExists <- lift $ doesDirectoryExist "dce-output"
  when outputDirExists $
    lift $ removeDirectoryRecursive "dce-output"
  (ecZephyr, _, errZephyr) <- lift $ readProcessWithExitCode "zephyr" (["-O", "1"] ++ T.unpack `map` entryPoints) ""
  when (ecZephyr /= ExitSuccess) (throwError $ ZephyrError repo ecZephyr errZephyr)
  

runTestLib :: (Text, [Text], [Text]) -> ExceptT TestError IO ()
runTestLib (repo, npmModules, entryPoints) = do
  cloneRepo repo
  npmInstall repo npmModules
  bowerInstall repo
  pursCompile repo
  runZephyr repo entryPoints

  (ecNode, stdNode , errNode) <- lift
    $ readProcessWithExitCode
        "node"
        [ "-e"
        , "setTimeout(process.exit.bind(process , 0), 2000); require('./dce-output/Test.Main/index.js').main()"
        ]
        ""
  when (ecNode /= ExitSuccess) (throwError $ NodeError repo ecNode stdNode errNode)

runKarmaLib
  :: (Text, Text, Text)
  -> ExceptT TestError IO ()
runKarmaLib (repo, entry, main) = do
  cloneRepo repo
  npmInstall repo []
  bowerInstall repo
  pursCompile repo
  runZephyr repo [entry]

  (ecBundle, _, errBundle) <- lift $ readProcessWithExitCode
        "purs"
        [ "bundle"
        , "--main", T.unpack main
        , "dce-output/**/*.js"
        , "-o" , "karma/test.js"
        ]
        ""
  when (ecBundle /= ExitSuccess) (throwError $ PursBundleError repo ecBundle errBundle)

  (ecBrowserify, _, errBrowserify) <- lift $ readProcessWithExitCode
        "browserify"
        [ "-e", "karma/test.js"
        , "-i", "react/addons"
        , "-i", "react/lib/ReactContext"
        , "-i", "react/lib/ExecutionEnvironment"
        , "-o", "karma/index.js"
        ]
        ""
  when (ecBrowserify /= ExitSuccess) (throwError $ BrowserifyError repo ecBrowserify errBrowserify)

  (ecKarma, stdKarma, errKarma) <- lift $ readProcessWithExitCode
        "karma"
        [ "start"
        , "--single-run"
        ]
        ""
  when (ecKarma /= ExitSuccess) (throwError $ NodeError repo ecKarma stdKarma errKarma)

assertRuns
  :: (Text, [Text], [Text])
  -> Expectation
assertRuns l = do
  res <- runExceptT . runTestLib $ l
  when (either (not . isGitError) (const True) res)
    (setCurrentDirectory "..")
  assertEqual "should run" (Right ()) res

assertRunsKarma
  :: (Text, Text, Text)
  -> Expectation
assertRunsKarma l = do
  res <- runExceptT . runKarmaLib $ l
  when (either (not . isGitError) (const True) res)
    (setCurrentDirectory "..")
  assertEqual "should run" (Right ()) res

contribSpec :: Spec
contribSpec = do
  context "test libraries" $ 
    forM_ coreLibs $ \l@(repo, _, _) ->
        specify (T.unpack repo) $ assertRuns l

karmaSpec :: Spec
karmaSpec = 
  context "karma tests" $ 
    forM_ karmaLibs $ \l@(repo, _, _) ->
        specify (T.unpack repo) $ assertRunsKarma l


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestDCE.main

  createDirectoryIfMissing False ".temp"
  setCurrentDirectory ".temp"

  hspec contribSpec
  hspec karmaSpec
