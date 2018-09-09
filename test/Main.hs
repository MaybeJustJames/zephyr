{-# LANGUAGE CPP #-}
module Main
  ( main
  , coreLibSpec
  , karmaSpec
  , libSpec
  ) where

import           Prelude ()
import           Prelude.Compat hiding (exp)
import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.List (init, last)
import           Data.Foldable (forM_)
import           Data.Maybe (fromJust, fromMaybe, isJust, maybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory
                  ( createDirectoryIfMissing
                  , doesDirectoryExist
                  , doesFileExist
                  , removeDirectoryRecursive
                  , getCurrentDirectory
                  , setCurrentDirectory
                  )
import           System.Exit (ExitCode(..))
import           System.IO (hSetEncoding, stdout, stderr, utf8)
import           System.Process (readProcess, readProcessWithExitCode)
import           Test.Hspec
import           Test.HUnit (assertEqual)

import qualified TestDCECoreFn
import qualified TestDCEEval

test_prg  :: String
#ifdef TEST_WITH_CABAL
test_prg = "cabal"
#else
test_prg = "stack"
#endif

test_args :: [String]
test_args = ["exec", "zephyr", "--"]

data CoreLibTest = CoreLibTest
  { coreLibTestRepo :: Text
  -- ^ git repo
  , coreLibTestNpmModules :: [Text]
  -- ^ additional node modules to install
  , coreLibTestEntries :: [Text]
  -- ^ entry points for `zephyr`
  , coreLibZephyrOptions :: Maybe [Text]
  -- ^ zephyr options
  , coreLibTestJsCmd :: Maybe (Text, Text)
  -- ^ node script, expected output
  }

coreLibs :: [CoreLibTest]
coreLibs =
  [ CoreLibTest "https://github.com/alexmingoia/purescript-pux.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/bodil/purescript-smolder.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/bodil/purescript-signal.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/bodil/purescript-test-unit.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/slamdata/purescript-aff.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/slamdata/purescript-avar.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/slamdata/purescript-matryoshka.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/slamdata/purescript-routing.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/slamdata/purescript-routing.git" []
      ["Routing.matches"]
      Nothing
      (Just
        ( "console.log(Object.keys(require('./dce-output/Routing')))"
        , "[ 'hashes', 'matches', 'matches\\'', 'matchWith', 'hashChanged' ]"))
  , CoreLibTest "https://github.com/slamdata/purescript-search.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest
      "https://github.com/purescript/purescript-console.git"
      []
      ["Control.Monad.Eff.Console.log"]
      Nothing
      (Just
        ( "require('./dce-output/Control.Monad.Eff.Console').log('hello')()"
        , "hello"))
  , CoreLibTest "https://github.com/purescript/purescript-free.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-prelude.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest
      "https://github.com/purescript/purescript-partial.git"
      []
      ["Test.Main.main", "Test.Main.safely", "Test.Main.safely2"]
      Nothing
      (Just
        ( "var r = require('./dce-output/Test.Main'); console.log(r.safely == r.safely2)"
        , "true"))
  , CoreLibTest "https://github.com/purescript/purescript-arrays.git" [] ["Test.Main.main"] (Just ["-f"]) Nothing
  , CoreLibTest "https://github.com/purescript/purescript-control.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-enums.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-generics-rep.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-maps.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-record.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-refs.git"
      []
      ["Control.Monad.Eff.Ref.newRef", "Control.Monad.Eff.Ref.readRef", "Control.Monad.Eff.Ref.writeRef"]
      Nothing
      (Just 
        ( "console.log(Object.keys(require('./dce-output/Control.Monad.Eff.Ref')))"
        , "[ 'newRef', 'readRef', 'writeRef' ]"
        ))
  , CoreLibTest "https://github.com/purescript/purescript-strings.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-transformers.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-quickcheck.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript/purescript-unsafe-coerce.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-argonaut.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-argonaut-codecs.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-argonaut-core.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-argonaut-generic.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-argonaut-traversals.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-foreign-lens.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-handlebars.git" ["handlebars"] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-js-date.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-lens.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-profunctor-lenses.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-nullable.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-options.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-parsing.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-precise.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-string-parsers.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-strongcheck.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-unicode.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-js-timers.git" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "https://github.com/purescript-contrib/purescript-unsafe-reference.git" [] ["Test.Main.main"] Nothing Nothing
  ]

data LibTest = LibTest
  { libTestEntries :: [Text]
  , libTestZephyrOptions :: Maybe [Text]
  , libTestJsCmd :: Text
  , libTestShouldPass :: Bool
  -- ^ true if should run without error, false if should error
  }

libTests :: [LibTest]
libTests =
  [ LibTest ["Unsafe.Coerce.Test.unsafeX"] Nothing "require('./dce-output/Unsafe.Coerce.Test').unsafeX(1)(1);" True
  ]

data KarmaTest = KarmaTest
  { karmaTestRepo :: Text
  -- ^ git repo
  , karmaTestEntry :: Text
  -- ^ `zephyre entry point
  }

karmaTests :: [KarmaTest]
karmaTests = 
  [ KarmaTest "https://github.com/coot/purescript-react-hocs.git" "Test.Main.main"
  , KarmaTest "https://github.com/coot/purescript-react-redox.git""Test.Karma.Main.main"
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
  | JsCmdError Text Text
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
  show (JsCmdError exp got) = "expected:\n\n" ++ T.unpack exp ++ "\n\nbut got:\n\n" ++ T.unpack got ++ "\n"

isGitError :: TestError -> Bool
isGitError (GitError _ _ _) = True
isGitError _ = False

cloneRepo
  :: Text
  -> ExceptT TestError IO FilePath
cloneRepo coreLibTestRepo = do
  let dir = head $ T.splitOn "." $ last $ T.splitOn "/" coreLibTestRepo

  repoExist <- lift $ doesDirectoryExist $ T.unpack dir
  unless repoExist $ do
    (ecGit, _, errGc) <- lift $ readProcessWithExitCode "git" ["clone", "--depth", "1", T.unpack coreLibTestRepo, T.unpack dir] ""
    when (ecGit /= ExitSuccess) (throwError (GitError coreLibTestRepo ecGit errGc))
  return (T.unpack dir)

npmInstall
  :: Text
  -> [Text]
  -> ExceptT TestError IO ()
npmInstall coreLibTestRepo npmModules = do
  pkgJson <- lift $ doesFileExist "package.json"
  nodeModulesExists <- lift $ doesDirectoryExist "node_modules"
  when ((pkgJson || not (null npmModules)) && not nodeModulesExists) $ do
    when (not $ null $ npmModules) $ do
      (ecNpm, _, errNpm) <- lift $ readProcessWithExitCode "npm" (["install"] ++ T.unpack `map` npmModules) ""
      when (ecNpm /= ExitSuccess) (throwError (NpmError coreLibTestRepo ecNpm errNpm))
    (ecNpm, _, errNpm) <- lift $ readProcessWithExitCode "npm" ["install"] ""
    when (ecNpm /= ExitSuccess) (throwError (NpmError coreLibTestRepo ecNpm errNpm))

bowerInstall
  :: Text
  -> ExceptT TestError IO ()
bowerInstall coreLibTestRepo = do
  bowerComponentsExists <- lift $ doesDirectoryExist "bower_components"
  when (not bowerComponentsExists) $ do
    (ecBower, _, errBower) <- lift $ readProcessWithExitCode "bower" ["install"] ""
    when (ecBower /= ecBower) (throwError (BowerError coreLibTestRepo ecBower errBower))

pursCompile
  :: Text
  -> ExceptT TestError IO ()
pursCompile coreLibTestRepo = do
  outputDirExists <- lift $ doesDirectoryExist "output"
  when (not outputDirExists) $ do
    (ecPurs, _, errPurs) <- lift
      $ readProcessWithExitCode
          "purs"
          [ "compile"
          , "--codegen" , "corefn"
          , "bower_components/purescript-*/src/**/*.purs"
          , "src/**/*.purs"
          , "test/**/*.purs"
          ]
          ""
    when (ecPurs /= ExitSuccess) (throwError $ PursError coreLibTestRepo ecPurs errPurs)

runZephyr
  :: Text
  -> [Text]
  -> Maybe [Text]
  -> ExceptT TestError IO ()
runZephyr coreLibTestRepo coreLibTestEntries zephyrOptions = do
  outputDirExists <- lift $ doesDirectoryExist "dce-output"
  when outputDirExists $
    lift $ removeDirectoryRecursive "dce-output"
  (ecZephyr, _, errZephyr) <- lift $ readProcessWithExitCode test_prg (test_args ++ T.unpack `map` fromMaybe ["-f"] zephyrOptions ++ T.unpack `map` coreLibTestEntries) ""
  when (ecZephyr /= ExitSuccess) (throwError $ ZephyrError coreLibTestRepo ecZephyr errZephyr)
  

runCoreLibTest :: CoreLibTest -> ExceptT TestError IO ()
runCoreLibTest (CoreLibTest {..}) = do
  dir <- cloneRepo coreLibTestRepo
  lift $ setCurrentDirectory dir
  npmInstall coreLibTestRepo coreLibTestNpmModules
  bowerInstall coreLibTestRepo
  pursCompile coreLibTestRepo
  runZephyr coreLibTestRepo coreLibTestEntries coreLibZephyrOptions

  (ecNode, stdNode, errNode) <- lift
    $ readProcessWithExitCode
        "node"
        [ "-e"
        , T.unpack $ maybe defaultJsCmd fst coreLibTestJsCmd
        ]
        ""

  lift $ setCurrentDirectory ".."

  when (ecNode /= ExitSuccess)
    (throwError $ NodeError coreLibTestRepo ecNode stdNode errNode)
  when (isJust coreLibTestJsCmd && Just (T.strip $ T.pack stdNode) /= (snd <$> coreLibTestJsCmd))
    (throwError $ JsCmdError (fromJust $ snd <$> coreLibTestJsCmd) (T.pack stdNode))

  where
    defaultJsCmd = "setTimeout(process.exit.bind(process , 0), 2000); require('./dce-output/Test.Main/index.js').main()"

runLibTest
  :: LibTest
  -> ExceptT TestError IO ()
runLibTest (LibTest {..}) = do
  bowerInstall "LibTest"
  pursCompile "LibTest"
  runZephyr "LibTest" libTestEntries libTestZephyrOptions
  (ecNode, stdNode, errNode) <- lift
    $ readProcessWithExitCode
        "node"
        [ "-e"
        , T.unpack libTestJsCmd
        ]
        ""
  when (libTestShouldPass && ecNode /= ExitSuccess)
    (throwError $ NodeError "LibTest (should pass)" ecNode stdNode errNode)
  when (not libTestShouldPass && ecNode == ExitSuccess)
    (throwError $ NodeError "LibTest (should fail)" ecNode stdNode errNode)

runKarmaTest
  :: KarmaTest
  -> ExceptT TestError IO ()
runKarmaTest KarmaTest{..} = do
  dir <- cloneRepo karmaTestRepo
  lift $ setCurrentDirectory dir
  npmInstall karmaTestRepo []
  bowerInstall karmaTestRepo
  pursCompile karmaTestRepo
  runZephyr karmaTestRepo [karmaTestEntry] Nothing

  (ecBundle, _, errBundle) <- lift $ readProcessWithExitCode
        "purs"
        [ "bundle"
        , "--main", T.unpack (T.intercalate "." . init . T.splitOn "." $ karmaTestEntry)
        , "dce-output/**/*.js"
        , "-o" , "karma/test.js"
        ]
        ""
  lift $ setCurrentDirectory ".."
  when (ecBundle /= ExitSuccess) (throwError $ PursBundleError karmaTestRepo ecBundle errBundle)

  (ecBrowserify, _, errBrowserify) <- lift $ readProcessWithExitCode
        "browserify"
        [ "-e", "karma/test.js"
        , "-i", "react/addons"
        , "-i", "react/lib/ReactContext"
        , "-i", "react/lib/ExecutionEnvironment"
        , "-o", "karma/index.js"
        ]
        ""
  when (ecBrowserify /= ExitSuccess) (throwError $ BrowserifyError karmaTestRepo ecBrowserify errBrowserify)

  (ecKarma, stdKarma, errKarma) <- lift $ readProcessWithExitCode
        "karma"
        [ "start"
        , "--single-run"
        ]
        ""
  when (ecKarma /= ExitSuccess) (throwError $ NodeError karmaTestRepo ecKarma stdKarma errKarma)

assertCoreLib
  :: CoreLibTest
  -> Expectation
assertCoreLib l = do
  res <- runExceptT . runCoreLibTest $ l
  assertEqual "core lib should run" (Right ()) res

assertLib
  :: LibTest
  -> Expectation
assertLib l = do
  res <- runExceptT . runLibTest $ l
  assertEqual "lib should run" (Right ()) res

assertKarma
  :: KarmaTest
  -> Expectation
assertKarma l = do
  res <- runExceptT . runKarmaTest $ l
  when (either (not . isGitError) (const True) res)
    (setCurrentDirectory "..")
  assertEqual "karma should run" (Right ()) res

coreLibSpec :: Spec
coreLibSpec = do
  context "test libraries" $ 
    forM_ coreLibs $ \l@(CoreLibTest repo  _ _ _ _) ->
        specify (T.unpack repo) $ assertCoreLib l

libSpec :: Spec
libSpec = do
  context "TestLib" $
    forM_ libTests $ \l ->
      specify (T.unpack $ T.intercalate (T.pack " ") $ libTestEntries l) $ assertLib l

karmaSpec :: Spec
karmaSpec = 
  context "karma tests" $ 
    forM_ karmaTests $ \l@(KarmaTest repo _) ->
        specify (T.unpack repo) $ assertKarma l

changeDir :: FilePath -> Spec -> Spec
changeDir path = around_
  $ \runTests -> do
      createDirectoryIfMissing False path
      cwd <- getCurrentDirectory
      setCurrentDirectory path
      runTests
      setCurrentDirectory cwd

main :: IO ()
main = do
  readProcess "purs" ["--version"] "" >>= putStrLn . (\v -> "\npurs version: " ++ v)

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestDCECoreFn.main
  TestDCEEval.main

  hspec $ changeDir "test/tests" libSpec
  -- hspec $ changeDir ".temp" coreLibSpec
  -- hspec karmaSpec
