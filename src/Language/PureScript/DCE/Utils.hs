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

bindIdents :: Bind Ann -> [Ident]
bindIdents (NonRec _ i _) = [i]
bindIdents (Rec bs) = (snd . fst) `map` bs

unApp :: Expr Ann -> (Expr Ann, [Expr Ann])
unApp e = go e []
  where
  go (App _ val arg) args = go val (arg : args)
  go other args = (other, args)

-- Check that `Qualified Ident` is shadowed by an identifier.
isShadowed :: ModuleName -> Qualified Ident -> Ident -> Bool
isShadowed mn qi@(Qualified mn' i') i =
  if Just mn == mn' || isUnqualified qi
    then i == i'
    else False

identFromVal :: Expr Ann -> Maybe (Qualified Ident)
identFromVal (Var _ i) = Just i
identFromVal _ = Nothing

everywhereOnAppM_ :: Monad m => (Ann -> Expr Ann -> Expr Ann -> m ()) -> Expr Ann -> m ()
everywhereOnAppM_ onApp = \case
  Literal _ (ArrayLiteral es) -> mapM_ (everywhereOnAppM_ onApp) es
  Literal _ (ObjectLiteral es) -> mapM_ (everywhereOnAppM_ onApp . snd) es
  Literal _ _ -> return ()
  Constructor _ _ _ _ -> return ()
  Accessor _ _ e -> everywhereOnAppM_ onApp e
  ObjectUpdate _ e es -> everywhereOnAppM_ onApp e *> mapM_ (everywhereOnAppM_ onApp . snd) es
  Abs _ _ e -> everywhereOnAppM_ onApp e
  App ann e1 e2 -> onApp ann e1 e2
  Var _ _ -> return ()
  Case _ es cs -> mapM_ (everywhereOnAppM_ onApp) es *> mapM_ (mapCaseAlternativeM_ (everywhereOnAppM_ onApp)) cs
  Let _ bs e -> mapM_ (mapBindM_ (everywhereOnAppM_ onApp)) bs *> everywhereOnAppM_ onApp e
