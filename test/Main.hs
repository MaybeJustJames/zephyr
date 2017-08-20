{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Prelude ()
import           Prelude.Compat
import           Control.Monad (join, when)
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.List (last)
import           Data.Foldable (forM_)
import           Data.Maybe (fromJust, isJust, maybe)
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

data CoreLibArg = CoreLibArg
  { coreLibArgRepo :: Text
  -- ^ git repo
  , coreLibArgNpmModules :: [Text]
  -- ^ additional node modules to install
  , coreLibArgEntries :: [Text]
  -- ^ entry points for `zephyr`
  , coreLibArgJsCmd :: Maybe (Text, Text)
  -- ^ node script, expected output
  }

coreLibs :: [CoreLibArg]
coreLibs =
  [ CoreLibArg "git@github.com:alexmingoia/purescript-pux" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:bodil/purescript-smolder" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:bodil/purescript-signal" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:bodil/purescript-test-unit" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:slamdata/purescript-aff" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:slamdata/purescript-matryoshka" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:slamdata/purescript-routing" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:slamdata/purescript-search" [] ["Test.Main.main"] Nothing
  , CoreLibArg
      "git@github.com:purescript/purescript-console"
      []
      ["Control.Monad.Eff.Console.log"]
      (Just
        ( "require('./dce-output/Control.Monad.Eff.Console').log('hello')()"
        , "hello\n"))
  , CoreLibArg "git@github.com:purescript/purescript-free" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript/purescript-prelude" [] ["Test.Main.main"] Nothing
  , CoreLibArg
      "git@github.com:purescript/purescript-partial"
      []
      ["Test.Main.main", "Test.Main.safely", "Test.Main.safely2"]
      (Just
        ( "var r = require('./dce-output/Test.Main'); console.log(r.safely == r.safely2)"
        , "true\n"))
  , CoreLibArg "git@github.com:purescript/purescript-quickcheck" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript/purescript-unsafe-coerce" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-argonaut" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-argonaut-codecs" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-argonaut-core" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-argonaut-generic" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-argonaut-traversals" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-foreign-lens" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-handlebars" ["handlebars"] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-js-date" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-lens" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-profunctor-lenses" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-nullable" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-options" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-parsing" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-precise" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-string-parsers" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-strongcheck" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-unicode" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-js-timers" [] ["Test.Main.main"] Nothing
  , CoreLibArg "git@github.com:purescript-contrib/purescript-unsafe-reference" [] ["Test.Main.main"] Nothing
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
cloneRepo coreLibArgRepo = do
  let dir = last $ T.splitOn "/" coreLibArgRepo

  repoExist <- lift $ doesDirectoryExist $ T.unpack dir
  when (not repoExist) $ do
    (ecGit, _, errGc) <- lift $ readProcessWithExitCode "git" ["clone", "--depth", "1", T.unpack coreLibArgRepo, T.unpack dir] ""
    when (ecGit /= ExitSuccess) (throwError (GitError coreLibArgRepo ecGit errGc))
  lift $ setCurrentDirectory (T.unpack dir)

npmInstall
  :: Text
  -> [Text]
  -> ExceptT TestError IO ()
npmInstall coreLibArgRepo npmModules = do
  npmInstall <- lift $ doesFileExist "package.json"
  nodeModulesExists <- lift $ doesDirectoryExist "node_modules"
  when ((npmInstall || not (null npmModules)) && not nodeModulesExists) $ do
    when (not $ null $ npmModules) $ do
      (ecNpm, _, errNpm) <- lift $ readProcessWithExitCode "npm" (["install"] ++ T.unpack `map` npmModules) ""
      when (ecNpm /= ExitSuccess) (throwError (NpmError coreLibArgRepo ecNpm errNpm))
    (ecNpm, _, errNpm) <- lift $ readProcessWithExitCode "npm" ["install"] ""
    when (ecNpm /= ExitSuccess) (throwError (NpmError coreLibArgRepo ecNpm errNpm))

bowerInstall
  :: Text
  -> ExceptT TestError IO ()
bowerInstall coreLibArgRepo = do
  bowerComponentsExists <- lift $ doesDirectoryExist "bower_components"
  when (not bowerComponentsExists) $ do
    (ecBower, _, errBower) <- lift $ readProcessWithExitCode "bower" ["install"] ""
    when (ecBower /= ecBower) (throwError (BowerError coreLibArgRepo ecBower errBower))

pursCompile
  :: Text
  -> ExceptT TestError IO ()
pursCompile coreLibArgRepo = do
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
    when (ecPurs /= ExitSuccess) (throwError $ PursError coreLibArgRepo ecPurs errPurs)

runZephyr
  :: Text
  -> [Text]
  -> ExceptT TestError IO ()
runZephyr coreLibArgRepo coreLibArgEntries = do
  outputDirExists <- lift $ doesDirectoryExist "dce-output"
  when outputDirExists $
    lift $ removeDirectoryRecursive "dce-output"
  (ecZephyr, _, errZephyr) <- lift $ readProcessWithExitCode "stack" (["exec", "zephyr", "--", "-O", "1"] ++ T.unpack `map` coreLibArgEntries) ""
  when (ecZephyr /= ExitSuccess) (throwError $ ZephyrError coreLibArgRepo ecZephyr errZephyr)
  

runTestLib :: CoreLibArg -> ExceptT TestError IO ()
runTestLib (CoreLibArg {..}) = do
  cloneRepo coreLibArgRepo
  npmInstall coreLibArgRepo coreLibArgNpmModules
  bowerInstall coreLibArgRepo
  pursCompile coreLibArgRepo
  runZephyr coreLibArgRepo coreLibArgEntries

  (ecNode, stdNode, errNode) <- lift
    $ readProcessWithExitCode
        "node"
        [ "-e"
        , T.unpack $ maybe defaultJsCmd fst coreLibArgJsCmd
        ]
        ""

  when (ecNode /= ExitSuccess)
    (throwError $ NodeError coreLibArgRepo ecNode stdNode errNode)
  when (isJust coreLibArgJsCmd && Just (T.pack stdNode) /= (snd <$> coreLibArgJsCmd))
    (throwError $ JsCmdError (fromJust $ snd <$> coreLibArgJsCmd) (T.pack stdNode))

  where
    defaultJsCmd = "setTimeout(process.exit.bind(process , 0), 2000); require('./dce-output/Test.Main/index.js').main()"

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
  :: CoreLibArg
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
    forM_ coreLibs $ \l@(CoreLibArg repo  _ _ _) ->
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
