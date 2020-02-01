-- |
-- Helper module for `zephyr`.
module Command.DCEOptions where

import qualified Language.PureScript as P
import Language.PureScript.DCE.Errors (EntryPoint)

data DCEOptions = DCEOptions
  { dceEntryPoints       :: [EntryPoint]
  , dceInputDir          :: FilePath
  , dceOutputDir         :: FilePath
  , dceVerbose           :: Bool
  , dceForeign           :: Bool
  , dcePureScriptOptions :: P.Options
  , dceUsePrefix         :: Bool
  , dceJsonErrors        :: Bool
  , dceDoEval            :: Bool
  }
