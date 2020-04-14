module Test.Karma (spec) where

import           Prelude ()
import           Prelude.Compat hiding (exp)
import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.List (init)
import           Data.Foldable (forM_)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory (setCurrentDirectory)
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)
import           Test.Hspec
import           Test.HUnit (assertEqual)

import           Test.Utils


data KarmaTest = KarmaTest
  { karmaTestRepo :: Text
  -- ^ git repo
  , karmaTestEntry :: Text
  -- ^ zephyr entry point
  }

karmaTests :: [KarmaTest]
karmaTests = 
  [ KarmaTest "https://github.com/coot/purescript-react-hocs.git" "Test.Main.main"
  , KarmaTest "https://github.com/coot/purescript-react-redox.git""Test.Karma.Main.main"
  ]


runKarmaTest
  :: KarmaTest
  -> ExceptT TestError IO ()
runKarmaTest KarmaTest{ karmaTestRepo, karmaTestEntry } = do
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

assertKarma
  :: KarmaTest
  -> Expectation
assertKarma l = do
    res <- runExceptT . runKarmaTest $ l
    when (either (not . isGitError) (const True) res)
      (setCurrentDirectory "..")
    assertEqual "karma should run" (Right ()) res
  where
    isGitError :: TestError -> Bool
    isGitError (GitError _ _ _) = True
    isGitError _ = False

spec :: Spec
spec = 
  context "karma tests" $ 
    forM_ karmaTests $ \l@(KarmaTest repo _) ->
        specify (T.unpack repo) $ assertKarma l
