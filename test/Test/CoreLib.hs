module Test.CoreLib (spec) where

import           Prelude ()
import           Prelude.Compat hiding (exp)
import           Control.Monad (when)
import           Control.Monad.Trans.Class
import           Control.Monad.Except
import           Data.Foldable (forM_)
import           Data.Maybe (fromJust, isJust, maybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory (setCurrentDirectory)
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)
import           Test.Hspec
import           Test.HUnit (assertEqual)

import           Test.Utils


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


runCoreLibTest :: CoreLibTest -> ExceptT TestError IO ()
runCoreLibTest CoreLibTest { coreLibTestRepo
                           , coreLibTestNpmModules
                           , coreLibTestEntries
                           , coreLibZephyrOptions
                           , coreLibTestJsCmd
                           } = do
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


assertCoreLib
  :: CoreLibTest
  -> Expectation
assertCoreLib l = do
  res <- runExceptT . runCoreLibTest $ l
  assertEqual "core lib should run" (Right ()) res


spec :: Spec
spec =
  changeDir ".temp" $  do
    context "test core libraries" $ 
      forM_ coreLibs $ \l@(CoreLibTest repo  _ _ _ _) ->
          specify (T.unpack repo) $ assertCoreLib l
