-- Dead Code Elimination for trivial Case expressions
module Language.PureScript.DCE.Eval
  ( dceEval ) where

import           Prelude.Compat
import           Control.Arrow (second)
import           Control.Applicative (liftA2, (<|>))
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Maybe (isJust, fromJust, fromMaybe, maybeToList)
import           Data.Monoid (First(..))
import           Language.PureScript.AST.Literals
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.DCE.Errors
import           Language.PureScript.DCE.Utils
import           Safe (atMay)

type Stack = [[(Ident, Expr Ann)]]

dceEval :: forall m t. (MonadError DCEError m) => [ModuleT t Ann] -> m [ModuleT t Ann]
dceEval mods = traverse go mods
  where
  go :: ModuleT t Ann -> m (ModuleT t Ann)
  go Module{..} = do
    decls <- (flip evalStateT (moduleName, []) . onBind') `traverse` moduleDecls
    return $ Module moduleComments moduleName modulePath moduleImports moduleExports moduleForeign decls

  (onBind', _) = everywhereOnValuesM onBind onExpr onBinders
    (modify $ second (drop 1))
    -- ^ pop recent value in the stack (it was added in `onBinders`

  onBind :: Bind Ann -> StateT (ModuleName, Stack) m (Bind Ann)
  onBind b = modify (second (unBind b :)) *> return b

  -- | 
  -- Track local identifiers in case binders, push them onto the stack.
  onBinders
    :: [Expr Ann]
    -> [Binder Ann]
    -> StateT (ModuleName, Stack) m [Binder Ann]
  onBinders es bs = do
    let bes = concatMap fn (zip bs es)
    modify (second (bes :))
    return bs
    where
    fn :: (Binder Ann, Expr Ann) -> [(Ident, Expr Ann)]
    fn (NullBinder _, _ ) = []
    fn (LiteralBinder _ _, _) = []
    fn (VarBinder _ i, e) = [(i,e)]
    fn (ConstructorBinder _ _ _ bs, e) = concatMap fn (zip bs (repeat e))
    fn (NamedBinder _ i b, e) = (i, e) : fn (b, e)

  -- |
  -- Evaluate expressions, keep the stack of local identifiers. It does not
  -- track identifiers which are coming from abstractions, but `Let` and
  -- `Case` binders are pushed into / poped from the stack.
  -- * `Let` binds are added in `onBind` and poped from the stack
  --   when visiting `Let` expression.
  -- * `Case` binds are added in `onBinders` and poped in the
  --  `everywhereOnValuesM` monadic action.
  onExpr
    :: Expr Ann
    -> StateT (ModuleName, Stack) m (Expr Ann)
  onExpr (Case ann es cs) = do
    es' <- map (join . fmap castToLiteral) <$> traverse eval es
    let cs' = getFirst $ foldMap (fndCase (fromJust `map` es')) cs
    if all isJust es'
      then case cs' of
        Nothing -> return $ Case ann es []
        Just (CaseAlternative bs (Right e))
          | not (any binds bs) -> return e
          | otherwise -> return $ Case ann es (maybeToList cs')
        Just (CaseAlternative bs (Left gs))
          -> do
            gs' <- fltGuards gs
            return $ Case ann es [CaseAlternative bs (Left gs')]
      else return $ Case ann es $ filter (fltBinders es' . caseAlternativeBinders) cs
    where
    fltGuards :: [(Guard Ann, Expr Ann)] -> StateT (ModuleName, Stack) m [(Guard Ann, Expr Ann)]
    fltGuards [] = return []
    fltGuards ((g,e):es) = do
      v <- eval g
      case v of
        Just (Literal _ t)
          | t `eqLit` BooleanLiteral True
          ->  return [(Literal (extractAnn g) (BooleanLiteral True), e)]
          | otherwise -- guard expression must evaluate to a Boolean
          -> fltGuards es
        _ -> ((g,e) :) <$> fltGuards es
  onExpr l@Let {} = modify (second (drop 1)) *> return l
  onExpr e = do
    (_, s) <- get
    v <- eval e
    case v of
      Just l  -> return l
      Nothing -> return e

  -- |
  -- Evaluate an expression
  -- * `Data.Eq.eq` of two literals
  -- * `Data.Array.index` on a literal array
  -- * Object accessors
  eval :: Expr Ann -> StateT (ModuleName, Stack) m (Maybe (Expr Ann))
  eval (Var _ (Qualified Nothing i)) = do
    (_, s) <- get
    join <$> traverse eval (fnd i s)
    where
      fnd :: Ident -> Stack -> Maybe (Expr Ann)
      fnd i s = getFirst $ foldMap (First . lookup i) s
  eval (Var ann qi@(Qualified (Just mn) i)) = do
    (cmn, _) <- get
    case findExpr mn i of
      Nothing -> throwError (IdentifierNotFound cmn ann qi)
      Just (Right e)  -> eval e
      Just (Left _)   -> return Nothing
  eval e@Literal{} = return (Just e)
  eval
    (App ann
      (App _
        (App _
          (Var _ (Qualified (Just (ModuleName [ProperName "Data", ProperName "Eq"])) (Ident "eq")))
          (Var _ inst))
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
        then do
          v1 <- eval e1
          v2 <- eval e2
          case (v1, v2) of
            (Just (Literal _ l1), Just (Literal _ l2))
              -> return $ Just $ Literal ann  $ BooleanLiteral (eqLit l1 l2)
            (_, _)
              -> return Nothing
        else return Nothing
    where
      mn = ModuleName [ProperName "Data", ProperName "Eq"]
  eval (Accessor ann a (Literal _ (ObjectLiteral as))) = do
    (mn, _) <- get
    e <- maybe (throwError (AccessorNotFound mn ann a)) return (a `lookup` as)
    eval e
  eval (App _
          (App _
            (Var ann@(ss, _, _, _) (Qualified (Just (ModuleName [ProperName "Data", ProperName "Array"])) (Ident "index")))
            (Literal _ (ArrayLiteral as)))
          (Literal _ (NumericLiteral (Left x))))
    = do
      (mn, _) <- get
      e <- maybe (throwError (ArrayIdxNotFound mn ann x)) return (as `atMay` fromIntegral x) >>= eval
      return
          $ App ann
              (Var (ss, [], Nothing, Just (IsConstructor SumType [Ident "value0"])) (Qualified (Just (ModuleName [ProperName "Data", ProperName "Maybe"])) (Ident "Just")))
        <$> e
  eval _ = return Nothing

  eqLit :: Literal a -> Literal b -> Bool
  eqLit (NumericLiteral (Left a)) (NumericLiteral (Left b)) = a == b
  eqLit (NumericLiteral (Right a)) (NumericLiteral (Right b)) = a == b
  eqLit (StringLiteral a) (StringLiteral b) = a == b
  eqLit (CharLiteral a) (CharLiteral b) = a == b
  eqLit (BooleanLiteral a) (BooleanLiteral b) = a == b
  eqLit _ _ = False

  fltBinders :: [Maybe (Literal (Expr Ann))] -> [Binder Ann] -> Bool
  fltBinders (Just l1 : ts) (LiteralBinder _ l2 : bs) = l1 `eqLit` l2 && fltBinders ts bs
  fltBinders _ _ = True

  -- |
  -- Cast an expression to a literal.
  castToLiteral :: Expr Ann -> Maybe (Literal (Expr Ann))
  castToLiteral (Literal _ l) = Just l
  castToLiteral _ = Nothing

  fndCase :: [Literal (Expr Ann)] -> CaseAlternative Ann -> First (CaseAlternative Ann)
  fndCase as c =
    if matches as (caseAlternativeBinders c)
      then First (Just c)
      else First Nothing
    where
    matches :: [Literal (Expr Ann)] -> [Binder Ann] -> Bool
    matches [] _ = True
    matches _ [] = True
    matches (t:ts) (LiteralBinder _ t' : bs) = t `eqLit` t' && matches ts bs
    matches (t:ts) (NamedBinder _ _ (LiteralBinder _ t') : bs) = t `eqLit` t' && matches ts bs
    matches (_:ts) (_:bs) = matches ts bs

  -- Does a binder binds?
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
  binds NamedBinder{} = True

  -- |
  -- Find a qualified name in the list of modules `mods`, return `Left ()` for
  -- `Prim` values, generics and foreign imports, `Right` for found bindings.
  findExpr :: ModuleName -> Ident -> Maybe (Either () (Expr Ann))
  findExpr (ModuleName (ProperName "Prim" : _)) _ = Just (Left ())
  findExpr (ModuleName [ProperName "Data", ProperName "Generic"]) (Ident "anyProxy") = Just (Left ())
  findExpr mn i
      = Right <$> join (getFirst . foldMap fIdent . concatMap unBind . moduleDecls <$> mod)
    <|> Left  <$> join (getFirst . foldMap ffIdent . moduleForeign <$> mod)
    where
    mod :: Maybe (ModuleT t Ann)
    mod = getFirst $ foldMap (\mod@(Module _ mn' _ _ _ _ _) -> if mn' == mn then First (Just mod) else First Nothing) mods

    fIdent :: (Ident, Expr Ann) -> First (Expr Ann)
    fIdent (i', e) | i == i'    = First (Just e)
                   | otherwise  = First Nothing

    ffIdent :: (Ident, t) -> First ()
    ffIdent (i', _) | i == i'   = First (Just ())
                    | otherwise = First Nothing
