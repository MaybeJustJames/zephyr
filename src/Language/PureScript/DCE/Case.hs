-- Dead Code Elimination for trivial Case expressions
module Language.PureScript.DCE.Case
  ( dceCase ) where

import           Prelude.Compat
import           Control.Arrow (second)
import           Control.Applicative (liftA2)
import           Control.Monad
import           Control.Monad.State
import           Data.Maybe (isJust, fromJust, fromMaybe, maybeToList)
import           Data.Monoid (First(..))
import           Language.PureScript.AST.Literals
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.DCE.Utils

type Stack = [[(Ident, Expr Ann)]]

dceCase :: forall t. [ModuleT t Ann] -> [ModuleT t Ann]
dceCase mods = map go mods
  where
  go Module{..} = Module moduleComments moduleName modulePath moduleImports moduleExports moduleForeign ((flip evalState [] . onBind') `map` moduleDecls)

  (onBind', _) = everywhereOnValuesM onBind onExpr onBinders
    (modify $ drop 1)

  onBind b = modify (unBind b :) *> return b

  onBinders es bs = do
    let bes = concatMap fn (zip bs es)
    modify (bes :)
    return bs
    where
    fn :: (Binder Ann, Expr Ann) -> [(Ident, Expr Ann)]
    fn (NullBinder _, _ ) = []
    fn (LiteralBinder _ _, _) = []
    fn (VarBinder _ i, e) = [(i,e)]
    fn (ConstructorBinder _ _ _ bs, e) = concatMap fn (zip bs (repeat e))
    fn (NamedBinder _ i b, e) = (i, e) : fn (b, e)

  onExpr (Case ann es cs) = do
    s <- get
    return $ onCase s ann es cs
  onExpr l@Let {} = modify (drop 1) *> return l
  onExpr e = do
    s <- get
    case eval s e of
      Just l  -> do
        return $ Literal (extractAnn e) l
      Nothing -> return e

  onCase :: Stack -> Ann -> [Expr Ann] -> [CaseAlternative Ann] -> Expr Ann
  onCase s ann es cs =
    let x = all isJust es'
    in if all isJust es'
      then case cs1 of
        Nothing -> Case ann es []
        Just (CaseAlternative bs (Right e))
          | not (any binds bs) -> e
          | otherwise -> Case ann es (maybeToList cs1)
        Just (CaseAlternative bs (Left gs))
          -> Case ann es [CaseAlternative bs (Left $ fltGuards gs)]
      else Case ann es cs2
    where
    es' = map (eval s) es

    cs1 = getFirst $ foldMap (fndCase (fromJust `map` es')) cs
    cs2 = filter (fltBinders es' . caseAlternativeBinders) cs

    fltGuards :: [(Guard Ann, Expr Ann)] -> [(Guard Ann, Expr Ann)]
    fltGuards [] = []
    fltGuards ((g,e):es) = case eval s g of
      Nothing 
        -> (g,e) : fltGuards es
      Just t
        | t `eqLit` (BooleanLiteral True)  
        ->  (Literal (extractAnn g) (BooleanLiteral True), e) : []
        | otherwise -- guard expression must evaluate to a Boolean
        -> fltGuards es

  eval :: Stack -> Expr Ann -> Maybe (Literal (Expr Ann))
  eval s (Var _ (Qualified Nothing i)) = 
    case fnd i s of
      Nothing -> Nothing
      Just e  -> eval s e
    where
      fnd :: Ident -> Stack -> Maybe (Expr Ann)
      fnd i s = getFirst $ foldMap (First . lookup i) s
  eval s (Var _ (Qualified (Just mn) i)) = join $ eval s <$> findExpr mn i mods
  eval _ (Literal _ (BooleanLiteral a)) = Just (BooleanLiteral a)
  eval _ (Literal _ (NumericLiteral a)) = Just (NumericLiteral a)
  eval _ (Literal _ (StringLiteral a)) = Just (StringLiteral a)
  eval _ (Literal _ (CharLiteral a)) = Just (CharLiteral a)
  eval s
    (App _
      (App _
        (App _
          (Var _ (Qualified (Just (ModuleName [ProperName "Data", ProperName "Eq"])) (Ident "eq")))
          (Var _ inst))  -- any Eq instance
          e1)
      e2)
    = if inst `elem`
          [ Qualified (Just mn) (Ident "eqBoolean")
          , Qualified (Just mn) (Ident "eqInt")
          , Qualified (Just mn) (Ident "eqNumber")
          , Qualified (Just mn) (Ident "eqChar")
          , Qualified (Just mn) (Ident "eqString")
          , Qualified (Just mn) (Ident "eqUnit")
          , Qualified (Just mn) (Ident "eqVoid")
          ]
        then BooleanLiteral <$> liftA2 (eqLit) (eval s e1) (eval s e2)
        else Nothing
    where
      mn = ModuleName [ProperName "Data", ProperName "Eq"]
  eval _ _ = Nothing

  eqLit :: Literal a -> Literal b -> Bool
  eqLit (NumericLiteral (Left a)) (NumericLiteral (Left b)) = a == b
  eqLit (NumericLiteral (Right a)) (NumericLiteral (Right b)) = a == b
  eqLit (StringLiteral a) (StringLiteral b) = a == b
  eqLit (CharLiteral a) (CharLiteral b) = a == b
  eqLit (BooleanLiteral a) (BooleanLiteral b) = a == b
  eqLit _ _ = False

  fltBinders :: [Maybe (Literal (Expr Ann))] -> [Binder Ann] -> Bool
  fltBinders ((Just l1):ts) ((LiteralBinder _ l2):bs) = l1 `eqLit` l2 && fltBinders ts bs
  fltBinders _ _ = True

  fndCase :: [Literal (Expr Ann)] -> CaseAlternative Ann -> First (CaseAlternative Ann)
  fndCase as c =
    if matches as (caseAlternativeBinders c) 
      then First (Just c)
      else First Nothing
    where
    matches :: [Literal (Expr Ann)] -> [Binder Ann] -> Bool
    matches [] _ = True
    matches _ [] = True
    matches (t:ts) ((LiteralBinder _ t'):bs) = t `eqLit` t' && matches ts bs
    matches (t:ts) ((NamedBinder _ _ (LiteralBinder _ t')):bs) = t `eqLit` t' && matches ts bs
    matches (_:ts) (_:bs) = matches ts bs

  binds :: Binder Ann -> Bool
  binds (NullBinder _) = False
  binds (LiteralBinder _ (NumericLiteral _)) = False
  binds (LiteralBinder _ (StringLiteral _)) = False
  binds (LiteralBinder _ (CharLiteral _)) = False
  binds (LiteralBinder _ (BooleanLiteral _)) = False
  binds (LiteralBinder _ (ArrayLiteral bs)) = any binds bs
  binds (LiteralBinder _ (ObjectLiteral bs)) = any (binds . snd) bs
  binds (VarBinder _ _) = True
  binds (ConstructorBinder _ _ _ bs) = any binds bs
  binds (NamedBinder _ _ _) = True

  findExpr :: ModuleName -> Ident -> [ModuleT t Ann] -> Maybe (Expr Ann)
  findExpr mn i mods = join $ getFirst . foldMap fIdent . concatMap unBind . moduleDecls <$> mod
    where
    mod :: Maybe (ModuleT t Ann)
    mod = getFirst $ foldMap (\mod@(Module _ mn' _ _ _ _ _) -> if mn' == mn then First (Just mod) else First Nothing) mods

    fIdent :: (Ident, Expr Ann) -> First (Expr Ann)
    fIdent (i', e) | i == i'    = First (Just e)
                   | otherwise  = First Nothing
