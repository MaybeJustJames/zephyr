module Test.Lib (spec) where

import           Control.Monad.Except
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Prelude                   ()
import           Prelude.Compat            hiding (exp)
import           System.Exit               (ExitCode (..))
import           System.Process            (readProcessWithExitCode)
import           Test.HUnit                (assertEqual)
import           Test.Hspec

import           Test.Utils


data LibTest = LibTest
  { libTestEntries       :: [Text]
  , libTestZephyrOptions :: Maybe [Text]
  , libTestJs            :: Text
  , libTestShouldPass    :: Bool
  -- ^ true if it should run without error, false if it should error
  }


libTests :: [LibTest]
libTests =
  [ LibTest ["Unsafe.Coerce.Test.unsafeX"]
            Nothing
            (  "import { unsafeX } from './dce-output/Unsafe.Coerce.Test/index.js';"
            <> "unsafeX(1)(1);"
            )
            True
  , LibTest ["Foreign.Test.add"]
            Nothing
            (  "import { add } from './dce-output/Foreign.Test/index.js';"
            <> "add(1)(1);"
            )
            True
  , LibTest ["Foreign.Test.add"]
            Nothing
            (  "import { mult } from './dce-output/Foreign.Test/index.js';"
            <> "mult(1)(1);"
            )
            False
  , LibTest ["Eval.makeAppQueue"]
            Nothing
            "import { makeAppQueue } from './dce-output/Eval/index.js';"
            True
  , LibTest ["Eval.evalUnderArrayLiteral"]
            Nothing
            "import { evalUnderArrayLiteral } from './dce-output/Eval/index.js';"
            True
  , LibTest ["Eval.evalUnderObjectLiteral"]
            Nothing
            "import { evalUnderObjectLiteral } from './dce-output/Eval/index.js';"
            True
  , LibTest ["Eval.evalVars"]
            Nothing
            "import { evalVars } from './dce-output/Eval/index.js';"
            True
  , LibTest ["Eval"]
            Nothing
            "import { evalVars } from './dce-output/Eval/index.js';"
            True
  , LibTest ["Eval.recordUpdate"]
            Nothing
            (  "import * as E from './dce-output/Eval/index.js';"
            <> "var foo = E.recordUpdate({ foo: '', bar: 0 })(E.Foo.create('foo'));"
            <> "if (foo.foo != 'foo') {"
            <> "  console.error(foo);"
            <> "  throw 'Error';"
            <> "}"
            )
            True
  , LibTest ["Literals.fromAnArray"]
            Nothing
            (  "import * as lits from './dce-output/Literals/index.js';"
            <> "if (lits.fromAnArray == null || lits.AStr == null || lits.AInt != null) {"
            <> "  throw 'Error';
            <> "}"
            )
            True
  , LibTest ["Literals.fromAnObject"]
            Nothing
            (  "import * as lits from './dce-output/Literals/index.js';"
            <> "if (lits.fromAnObject == null || lits.AStr == null || lits.AInt != null) {"
            <> "  throw 'Error';
            <> "}"
            )
            True
  -- Control.Alt re-exports map from Data.Functor
  , LibTest ["Control.Alt.map"]
            Nothing
            "import { map } from './dce-output/Control.Alt/index.js';"
            True
  , LibTest ["Data.Array.span"]
            Nothing
            "import { span } from './dce-output/Data.Array/index.js';"
            True
  ]


assertLib :: LibTest -> Expectation
assertLib l = do
  res <- runExceptT . runLibTest $ l
  assertEqual "lib should run" (Right ()) res


runLibTest :: LibTest -> ExceptT TestError IO ()
runLibTest LibTest { libTestEntries
                   , libTestZephyrOptions
                   , libTestJsFilename
                   , libTestShouldPass
                   } = do
  spagoBuild "LibTest"
  runZephyr "LibTest" libTestEntries libTestZephyrOptions
  (ecNode, stdNode, errNode) <- lift
    $ readProcessWithExitCode
        "node"
        [ "--input-type=module"
        , "-e"
        , T.unpack libTestJs
        ]
        ""
  when (libTestShouldPass && ecNode /= ExitSuccess)
    (throwError $ NodeError "LibTest (should pass)" ecNode stdNode errNode)
  when (not libTestShouldPass && ecNode == ExitSuccess)
    (throwError $ NodeError "LibTest (should fail)" ecNode stdNode errNode)


spec :: Spec
spec =
  changeDir "test/lib-tests" $
    context "test-lib" $
      forM_ libTests $ \l ->
        specify (T.unpack $ T.intercalate (T.pack " ") $ libTestEntries l) $ assertLib l
