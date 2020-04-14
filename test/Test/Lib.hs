module Test.Lib (spec) where

import           Prelude ()
import           Prelude.Compat hiding (exp)
import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.Foldable (forM_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Semigroup ((<>))
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)
import           Test.Hspec
import           Test.HUnit (assertEqual)

import           Test.Utils


data LibTest = LibTest
  { libTestEntries :: [Text]
  , libTestZephyrOptions :: Maybe [Text]
  , libTestJsCmd :: Text
  , libTestShouldPass :: Bool
  -- ^ true if it should run without error, false if it should error
  }


libTests :: [LibTest]
libTests =
  [ LibTest ["Unsafe.Coerce.Test.unsafeX"] Nothing "require('./dce-output/Unsafe.Coerce.Test').unsafeX(1)(1);" True
  , LibTest ["Foreign.Test.add"] Nothing "require('./dce-output/Foreign.Test').add(1)(1);" True
  , LibTest ["Foreign.Test.add"] Nothing "require('./dce-output/Foreign.Test/foreign.js').mult(1)(1);" False
  , LibTest ["Eval.makeAppQueue"] Nothing "require('./dce-output/Eval').makeAppQueue;" True
  , LibTest ["Eval.evalUnderArrayLiteral"] Nothing "require('./dce-output/Eval').evalUnderArrayLiteral;" True
  , LibTest ["Eval.evalUnderObjectLiteral"] Nothing "require('./dce-output/Eval').evalUnderObjectLiteral;" True
  , LibTest ["Eval.evalVars"] Nothing "require('./dce-output/Eval').evalVars;" True
  , LibTest ["Eval"] Nothing "require('./dce-output/Eval').evalVars;" True
  , LibTest ["Eval.recordUpdate"] Nothing
       ( " var eval = require('./dce-output/Eval');\n"
      <> " var foo = eval.recordUpdate({foo: '', bar: 0})(eval.Foo.create('foo'));\n"
      <> " if (foo.foo != 'foo') {\n"
      <> "    console.error(foo)\n"
      <> "    throw('Error')\n"
      <> " }\n"
      )
      True
  ]


assertLib :: LibTest -> Expectation
assertLib l = do
  res <- runExceptT . runLibTest $ l
  assertEqual "lib should run" (Right ()) res


runLibTest :: LibTest -> ExceptT TestError IO ()
runLibTest LibTest { libTestEntries
                   , libTestZephyrOptions
                   , libTestJsCmd
                   , libTestShouldPass
                   } = do
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


spec :: Spec
spec =
  changeDir "test/lib-tests" $
    context "TestLib" $
      forM_ libTests $ \l ->
        specify (T.unpack $ T.intercalate (T.pack " ") $ libTestEntries l) $ assertLib l
