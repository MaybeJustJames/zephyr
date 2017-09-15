module Language.PureScript.DCE.Utils where

import           Prelude.Compat
import           Control.Arrow (first)
import           Language.PureScript.CoreFn
import           Language.PureScript.Names

bindIdents :: Bind Ann -> [Ident]
bindIdents (NonRec _ i _) = [i]
bindIdents (Rec bs) = (snd . fst) `map` bs

bindExprs :: Bind Ann -> [Expr Ann]
bindExprs (NonRec _ _ e) = [e]
bindExprs (Rec bs) = snd `map` bs

unBind :: Bind Ann -> [(Ident, Expr Ann)]
unBind (NonRec _ i e) = [(i, e)]
unBind (Rec bs) = first snd `map` bs

everywhereOnValuesM
  :: forall m a
   . (Monad m)
  => (Bind a -> m (Bind a))
  -> (Expr a -> m (Expr a))
  -> ([Expr a] -> [Binder a] -> m [Binder a])
  -> m ()
  -- ^ monadic computation fired after handling case alternative
  -> (Bind a -> m (Bind a), Expr a -> m (Expr a))
everywhereOnValuesM f g h mh = (f', g')
  where
  f' (NonRec a name e) = NonRec a name <$> g' e >>= f
  f' (Rec es) = Rec <$> traverse (traverse g) es >>= f
     
  g' (Literal ann e) = Literal ann <$> (handleLiteral g' e) >>= g
  g' (Accessor ann prop e) = Accessor ann prop <$> (g' e) >>= g
  g' (ObjectUpdate ann obj vs) = ObjectUpdate ann <$> g' obj <*> traverse (traverse g') vs >>= g
  g' (Abs ann name e) = Abs ann name <$> (g' e) >>= g
  g' app@(App ann v1 v2) = App ann <$> g' v1 <*> g' v2 >>= g
  g' (Case ann vs alts) = do
    vs' <- traverse g' vs
    alts' <- traverse (handleCaseAlternative vs') alts
    g (Case ann vs' alts')
  g' (Let ann ds e) = Let ann <$> traverse f' ds <*> g' e >>= g
  g' e = g e

  handleCaseAlternative :: [Expr a] -> CaseAlternative a -> m (CaseAlternative a)
  handleCaseAlternative es (CaseAlternative bs r) = do
    bs <- h es bs
    rs <- g'' r
    mh
    return (CaseAlternative bs rs)
    where
    g'' :: Either [(Guard a, Expr a)] (Expr a) -> m (Either [(Guard a, Expr a)] (Expr a))
    g'' (Left es) = Left <$> traverse gn es
    g'' (Right e) = Right <$> g' e

    gn (e1, e2) = (,) <$> g' e1 <*> g' e2

  handleLiteral :: (b -> m b) -> Literal b -> m (Literal b)
  handleLiteral i (ArrayLiteral ls) = ArrayLiteral <$> (traverse i ls)
  handleLiteral i (ObjectLiteral ls) = ObjectLiteral <$> (traverse (traverse i) ls)
  handleLiteral _ other = return other
