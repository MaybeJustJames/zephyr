module Language.PureScript.DCE.Errors
  ( DCEError(..)
  , displayDCEError
  )
  where

import Prelude.Compat

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Language.PureScript.AST.SourcePos (SourceSpan(..), displaySourceSpan)
import Language.PureScript.Names
import Language.PureScript.PSString
import Language.PureScript.CoreFn.Ann

data DCEError
  = IdentifierNotFound ModuleName Ann (Qualified Ident)
  | AccessorNotFound ModuleName Ann PSString
  | ArrayIdxNotFound ModuleName Ann Integer
  deriving (Show)

displaySourcePos :: FilePath -> ModuleName -> SourceSpan -> Text
displaySourcePos relPath mn ss
   = "in module " <> runModuleName mn <> "\n"
  <> "at " <> displaySourceSpan relPath ss <> "\n"

displayDCEError :: FilePath -> DCEError -> Text
displayDCEError relPath (IdentifierNotFound mn (ss, _, _, _) qi)
   = displaySourcePos relPath mn ss
  <> "\t" <> "Unknown value " <> showQualified runIdent qi <> ".\n"
displayDCEError relPath (AccessorNotFound mn (ss, _, _, _) acc)
   = displaySourcePos relPath mn ss
  <> "\t" <> "Type of expression lacks required label " <> prettyPrintString acc <> ".\n"
displayDCEError relPath (ArrayIdxNotFound mn (ss, _, _, _) i)
   = displaySourcePos relPath mn ss
  <> "\t" <> "Type of expression lacks required index " <> T.pack (show i) <> ".\n"
