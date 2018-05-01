module Language.PureScript.DCE.Options where

import Prelude.Compat

import qualified Data.Text as T
import qualified Language.PureScript as P

newtype EntryPoint = EntryPoint { runEntryPoint :: P.Qualified P.Ident }

instance Read EntryPoint where
  readsPrec _ s = case unsnoc (T.splitOn "." (T.pack s)) of
      Just (as, a) | not (null as)  -> [(EntryPoint (P.mkQualified (P.Ident a) (P.ModuleName $ P.ProperName <$> as)), "")]
                   | otherwise      -> [(EntryPoint (P.Qualified Nothing (P.Ident a)), "")]
      Nothing                       -> []
    where
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc [] = Nothing
    unsnoc as = Just (init as, last as)

data DCEOptions = DCEOptions
  { dceEntryPoints :: [EntryPoint]
  , dceInputDir :: FilePath
  , dceOutputDir :: FilePath
  , dceVerbose :: Bool
  , dceForeign :: Bool
  , dcePureScriptOptions :: P.Options
  , dceUsePrefix :: Bool
  , dceJsonErrors :: Bool
  }
