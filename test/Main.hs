module Main (main) where

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
                  , setCurrentDirectory
                  )
import           System.Exit (ExitCode(..))
import           System.IO (hSetEncoding, stdout, stderr, utf8)
import           System.Process (readProcessWithExitCode)
import           Test.Hspec
import           Test.HUnit (assertEqual)

import qualified TestDCECoreFn
import qualified TestDCEEval

data CoreLibTest = CoreLibTest
  { coreLibTestRepo :: Text
  -- ^ git repo
  , coreLibTestNpmModules :: [Text]
  -- ^ additional node modules to install
  , coreLibTestEntries :: [Text]
  -- ^ entry points for `zephyr`
  , zephyrOptions :: Maybe [Text]
  -- ^ zephyr options
  , coreLibTestJsCmd :: Maybe (Text, Text)
  -- ^ node script, expected output
  }

coreLibs :: [CoreLibTest]
coreLibs =
  [ CoreLibTest "git@github.com:alexmingoia/purescript-pux" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:bodil/purescript-smolder" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:bodil/purescript-signal" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:bodil/purescript-test-unit" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:slamdata/purescript-aff" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:slamdata/purescript-matryoshka" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:slamdata/purescript-routing" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:slamdata/purescript-routing" []
      ["Routing.matches"]
      Nothing
      (Just
        ( "console.log(Object.keys(require('./dce-output/Routing')))"
        , "[ 'hashes', 'matches', 'matches\\'', 'matchWith', 'hashChanged' ]"))
  , CoreLibTest "git@github.com:slamdata/purescript-search" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest
      "git@github.com:purescript/purescript-console"
      []
      ["Control.Monad.Eff.Console.log"]
      Nothing
      (Just
        ( "require('./dce-output/Control.Monad.Eff.Console').log('hello')()"
        , "hello"))
  , CoreLibTest "git@github.com:purescript/purescript-free" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-prelude" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest
      "git@github.com:purescript/purescript-partial"
      []
      ["Test.Main.main", "Test.Main.safely", "Test.Main.safely2"]
      Nothing
      (Just
        ( "var r = require('./dce-output/Test.Main'); console.log(r.safely == r.safely2)"
        , "true"))
  , CoreLibTest "git@github.com:purescript/purescript-arrays" [] ["Test.Main.main"] (Just ["-f"]) Nothing
  , CoreLibTest "git@github.com:purescript/purescript-control" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-free" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-enums" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-generics-rep" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-maps" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-record" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-refs"
      []
      ["Control.Monad.Eff.Ref.newRef", "Control.Monad.Eff.Ref.readRef", "Control.Monad.Eff.Ref.writeRef"]
      Nothing
      (Just 
        ( "console.log(Object.keys(require('./dce-output/Control.Monad.Eff.Ref')))"
        , "[ 'newRef', 'readRef', 'writeRef' ]"
        ))
  , CoreLibTest "git@github.com:purescript/purescript-strings" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-transformers" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-quickcheck" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript/purescript-unsafe-coerce" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-argonaut" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-argonaut-codecs" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-argonaut-core" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-argonaut-generic" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-argonaut-traversals" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-foreign-lens" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-handlebars" ["handlebars"] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-js-date" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-lens" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-profunctor-lenses" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-nullable" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-options" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-parsing" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-precise" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-string-parsers" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-strongcheck" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-unicode" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-js-timers" [] ["Test.Main.main"] Nothing Nothing
  , CoreLibTest "git@github.com:purescript-contrib/purescript-unsafe-reference" [] ["Test.Main.main"] Nothing Nothing
  ]

data KarmaTest = KarmaTest
  { karmaTestRepo :: Text
  -- ^ git repo
  , karmaTestEntry :: Text
  -- ^ `zephyre entry point
  }

karmaTests :: [KarmaTest]
karmaTests = 
  [ KarmaTest "git@github.com:coot/purescript-react-hocs" "Test.Main.main"
  , KarmaTest "git@github.com:coot/purescript-react-redox""Test.Karma.Main.main"
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
  -> ExceptT TestError IO ()
cloneRepo coreLibTestRepo = do
  let dir = last $ T.splitOn "/" coreLibTestRepo

  repoExist <- lift $ doesDirectoryExist $ T.unpack dir
  when (not repoExist) $ do
    (ecGit, _, errGc) <- lift $ readProcessWithExitCode "git" ["clone", "--depth", "1", T.unpack coreLibTestRepo, T.unpack dir] ""
    when (ecGit /= ExitSuccess) (throwError (GitError coreLibTestRepo ecGit errGc))
  lift $ setCurrentDirectory (T.unpack dir)

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
          , "--dump-corefn"
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
  (ecZephyr, _, errZephyr) <- lift $ readProcessWithExitCode "stack" (["exec", "zephyr", "--"] ++ T.unpack `map` fromMaybe ["-f"] zephyrOptions ++ T.unpack `map` coreLibTestEntries) ""
  when (ecZephyr /= ExitSuccess) (throwError $ ZephyrError coreLibTestRepo ecZephyr errZephyr)
  

runTestLib :: CoreLibTest -> ExceptT TestError IO ()
runTestLib (CoreLibTest {..}) = do
  cloneRepo coreLibTestRepo
  npmInstall coreLibTestRepo coreLibTestNpmModules
  bowerInstall coreLibTestRepo
  pursCompile coreLibTestRepo
  runZephyr coreLibTestRepo coreLibTestEntries zephyrOptions

  (ecNode, stdNode, errNode) <- lift
    $ readProcessWithExitCode
        "node"
        [ "-e"
        , T.unpack $ maybe defaultJsCmd fst coreLibTestJsCmd
        ]
        ""

  when (ecNode /= ExitSuccess)
    (throwError $ NodeError coreLibTestRepo ecNode stdNode errNode)
  when (isJust coreLibTestJsCmd && Just (T.strip $ T.pack stdNode) /= (snd <$> coreLibTestJsCmd))
    (throwError $ JsCmdError (fromJust $ snd <$> coreLibTestJsCmd) (T.pack stdNode))

  where
    defaultJsCmd = "setTimeout(process.exit.bind(process , 0), 2000); require('./dce-output/Test.Main/index.js').main()"

runKarmaTest
  :: KarmaTest
  -> ExceptT TestError IO ()
runKarmaTest KarmaTest{..} = do
  cloneRepo karmaTestRepo
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

assertRuns
  :: CoreLibTest
  -> Expectation
assertRuns l = do
  res <- runExceptT . runTestLib $ l
  when (either (not . isGitError) (const True) res)
    (setCurrentDirectory "..")
  assertEqual "should run" (Right ()) res

assertKarma
  :: KarmaTest
  -> Expectation
assertKarma l = do
  res <- runExceptT . runKarmaTest $ l
  when (either (not . isGitError) (const True) res)
    (setCurrentDirectory "..")
  assertEqual "should run" (Right ()) res

contribSpec :: Spec
contribSpec = do
  context "test libraries" $ 
    forM_ coreLibs $ \l@(CoreLibTest repo  _ _ _ _) ->
        specify (T.unpack repo) $ assertRuns l

karmaSpec :: Spec
karmaSpec = 
  context "karma tests" $ 
    forM_ karmaTests $ \l@(KarmaTest repo _) ->
        specify (T.unpack repo) $ assertKarma l


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestDCECoreFn.main
  TestDCEEval.main

  createDirectoryIfMissing False ".temp"
  setCurrentDirectory ".temp"

  hspec contribSpec
  hspec karmaSpec
