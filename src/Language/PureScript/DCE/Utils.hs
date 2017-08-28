module Language.PureScript.DCE.Utils where

import           Prelude.Compat
import           Data.Maybe (Maybe(..))
import qualified Language.PureScript as P
import           Language.PureScript.CoreFn
import           Language.PureScript.Names

getAnn :: Expr Ann -> Ann
getAnn (Literal a _) = a
getAnn (Constructor a _ _ _) = a
getAnn (Accessor a _ _) = a
getAnn (ObjectUpdate a _ _) = a
getAnn (Abs a _ _) = a
getAnn (App a _ _) = a
getAnn (Var a _) = a
getAnn (Case a _ _) = a
getAnn (Let a _ _) = a

-- | 
-- Check if an expression is constrained.
--
-- This requires types to be recoverable from [CoreFn
-- representation](https://github.com/coot/purescript/commit/29ef583ae754ca963dc245e8a1b63445f1eeb90e).
isConstrained :: Expr Ann -> Maybe P.Constraint
isConstrained (Abs (_, _, Just (P.ConstrainedType c _), _) _ _) = Just c
isConstrained _ = Nothing

-- |
-- split epi from `Ident` to `ProperName 'ClassName`
--
-- It must be right left inverse of
-- `Language.PureScript.CoreFn.Desugar.properToIdent`
identToProper :: Ident -> ProperName a
identToProper = ProperName . runIdent
