module Language.PureScript.DCE.Errors where

import           Data.Text (Text)
import           Language.PureScript.PSString (PSString)
import           Language.PureScript.Names

data DceError
  = TypeClassNotFound (Qualified (ProperName 'ClassName))
  | NotTypeClassMember (Qualified (ProperName 'ClassName)) PSString
  | TypeClassInstanceNotFound (Qualified Ident)
  | CoreFnUnexpectedNode Text
