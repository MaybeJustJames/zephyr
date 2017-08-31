module Language.PureScript.DCE.Utils where

import           Prelude.Compat
import           Control.Arrow ((***))
import           Control.Monad
import qualified Data.Either (Either(..))
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

getConstraint :: P.Type -> Maybe P.Constraint
getConstraint (P.ForAll _ ty _) = getConstraint ty
getConstraint (P.ConstrainedType c _) = Just c
getConstraint _ = Nothing

-- |
-- split epi from `Ident` to `ProperName 'ClassName`
--
-- It must be right left inverse of
-- `Language.PureScript.CoreFn.Desugar.properToIdent`
identToProper :: Ident -> ProperName a
identToProper = ProperName . runIdent

mapCaseAlternativeM_ :: Monad m => (Expr Ann -> m ()) -> CaseAlternative Ann -> m ()
mapCaseAlternativeM_ onExpr (CaseAlternative _ (Left gs))
  = mapM_ (uncurry (*>) . (onExpr *** onExpr)) gs
mapCaseAlternativeM_ onExpr (CaseAlternative _ (Right e)) = onExpr e

mapBindM_ :: Monad m => (Expr Ann -> m ()) -> Bind Ann -> m ()
mapBindM_ onExpr (NonRec a _ e) = onExpr e
mapBindM_ onExpr (Rec bs) = mapM_ (onExpr . snd) bs
