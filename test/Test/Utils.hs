{-# LANGUAGE CPP #-}

module Test.Utils where

import           Prelude ()
import           Prelude.Compat hiding (exp)
import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.List (last)
import           Data.Maybe (fromMaybe)
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
import           System.Process (readProcessWithExitCode)
import           Test.Hspec


test_prg  :: String; test_args :: [String]
#ifndef TEST_WITH_STACK
test_prg = "cabal"
test_args = ["run", "exe:zephyr", "--"]
#else
test_prg = "stack"
test_args = ["exec", "zephyr", "--"]
#endif


changeDir :: FilePath -> Spec -> Spec
changeDir path =
    around_ $ \runTests -> do
      createDirectoryIfMissing False path
      cwd <- getCurrentDirectory
      setCurrentDirectory path
      runTests
      setCurrentDirectory cwd


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


runZephyr
  :: Text
  -> [Text]
  -> Maybe [Text]
  -> ExceptT TestError IO ()
runZephyr coreLibTestRepo coreLibTestEntries zephyrOptions = do
  outputDirExists <- lift $ doesDirectoryExist "dce-output"
  when outputDirExists $
    lift $ removeDirectoryRecursive "dce-output"
  (ecZephyr, _, errZephyr) <-
    lift $
      readProcessWithExitCode test_prg
        (test_args ++ T.unpack `map` fromMaybe ["--evaluate", "--dce-foreign"] zephyrOptions ++ T.unpack `map` coreLibTestEntries)
        ""
  when (ecZephyr /= ExitSuccess) (throwError $ ZephyrError coreLibTestRepo ecZephyr errZephyr)


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
