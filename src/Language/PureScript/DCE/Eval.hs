-- |
-- Evaluation of PureScript's expressions used in dead call elimnation.
module Language.PureScript.DCE.Eval
  ( dceEval ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
import Language.PureScript.DCE.Errors
import Language.PureScript.DCE.Utils
import Language.PureScript.Names
import Language.PureScript.PSString

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Data.Maybe (Maybe(..), fromJust, isJust, maybeToList)
import Data.Monoid (First(..))
import qualified Data.Text as T
import qualified Language.PureScript.DCE.Constants as C
import Prelude.Compat hiding (mod)
import Safe (atMay)

data EvalState
  = NotYet -- ^ an expression has not yet been evaluated
  | Done   -- ^ an expression has been evaluated
  deriving (Eq, Show)

type Stack = [[(Ident, (Expr Ann, EvalState))]]

initStack :: [(Ident, Expr Ann)] -> [(Ident, (Expr Ann, EvalState))]
initStack = map (\(i, e) -> (i, (e, NotYet)))

-- Mark first found expression as evaluated to avoid infinite loops.
markDone :: Ident -> Stack -> Stack
markDone _ [] = []
markDone i (l : ls) =
  case foldr fn ([], False) l of
    (l', True)  -> l' : ls
    (l', False) -> l' : markDone i ls
  where
  fn (i', v) (is, done)
    | i == i' = ((i', (fst v, Done)) : is, True)
    | otherwise = ((i', v) : is, done)

-- |
-- Evaluate expressions in a module:
--
-- * @Data.Eq.eq@ of two literals
-- * @Data.Array.index@ on a literal array
-- * Object accessors
-- * Semigroup operations (@Array@, @String@, @Unit@)
-- * Semiring operations (@Unit@, @Unit@, @Unit@)
--
-- Keep stack of local identifiers from @let@ and @case@ expressions, ignoring
-- the ones that are comming from abstractions.
dceEval
  :: forall m
   . (MonadError (DCEError 'Error) m, MonadWriter [DCEError 'Warning] m)
  => [Module Ann]
  -> m [Module Ann]
dceEval mods = traverse go mods
  where
  go :: Module Ann -> m (Module Ann)
  go Module{..} = do
    decls <- (flip evalStateT (moduleName, []) . onBind') `traverse` moduleDecls
    return $ Module
      moduleSourceSpan
      moduleComments
      moduleName
      modulePath
      moduleImports
      moduleExports
      moduleForeign
      decls

  (onBind', _) = everywhereOnValuesM onBind onExpr onBinders
    (modify $ second (drop 1))
    -- pop recent value in the stack (it was added in `onBinders`)

  onBind :: Bind Ann -> StateT (ModuleName, Stack) m (Bind Ann)
  onBind b = modify (second (initStack (unBind b) :)) $> b

  -- |
  -- Track local identifiers in case binders, push them onto the stack.
  onBinders
    :: [Expr Ann]
    -> [Binder Ann]
    -> StateT (ModuleName, Stack) m [Binder Ann]
  onBinders es bs = do
    let bes = concatMap fn (zip bs es)
    modify (second (initStack bes :))
    return bs
    where
    fn :: (Binder Ann, Expr Ann) -> [(Ident, Expr Ann)]
    fn (NullBinder _, _ ) = []
    fn (LiteralBinder _ _, _) = []
    fn (VarBinder _ i, e) = [(i,e)]
    fn (ConstructorBinder _ _ _ as, e) = concatMap fn (zip as (repeat e))
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
    es' <- map (>>= castToLiteral) <$> traverse eval es
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
      else
        return
          $ Case ann es
          $ filter (fltBinders es' . caseAlternativeBinders) cs
    where
    fltGuards
      :: [(Guard Ann, Expr Ann)]
      -> StateT (ModuleName, Stack) m [(Guard Ann, Expr Ann)]
    fltGuards [] = return []
    fltGuards ((g,e):rest) = do
      v <- eval g
      case v of
        Just (Literal _ t)
          | t `eqLit` BooleanLiteral True
          ->  return [(Literal (extractAnn g) (BooleanLiteral True), e)]
          | otherwise -- guard expression must evaluate to a Boolean
          -> fltGuards rest
        _ -> ((g,e) :) <$> fltGuards rest
  onExpr l@Let{} = modify (second (drop 1)) $> l
  onExpr e@Var{} = do
    v <- eval e
    case v of
      Just l@(Literal _ NumericLiteral{}) -> return l
      Just l@(Literal _ CharLiteral{})    -> return l
      Just l@(Literal _ BooleanLiteral{}) -> return l
      -- preserve string, array and object literals
      Just _  -> return e
      Nothing -> return e
  onExpr e = do
    v <- eval e
    case v of
      Just l  -> return l
      Nothing -> return e

  -- |
  -- Evaluate an expression
  -- * `Data.Eq.eq` of two literals
  -- * `Data.Array.index` on a literal array
  -- * Object accessors
  -- * Semigroup operations (Array, String, Unit)
  -- * Semiring operations (Int, Number, Unit)
  -- * Heyting algebra operations (Boolean, Unit)
  eval :: Expr Ann -> StateT (ModuleName, Stack) m (Maybe (Expr Ann))
  eval (Var _ (Qualified Nothing i)) = do
    (_, s) <- get
    join <$> traverse eval' (fnd i s)
    where
      fnd :: Ident -> Stack -> Maybe (Expr Ann, EvalState)
      fnd j s = getFirst $ foldMap (First . lookup j) s

      eval' :: (Expr Ann, EvalState) -> StateT (ModuleName, Stack) m (Maybe (Expr Ann))
      eval' (e, Done) = return (Just e)
      eval' (e, _)    = do
        modify (\(mn, s) -> (mn, markDone i s))
        eval e
  eval (Var ann qi@(Qualified (Just mn) i)) = do
    (cmn, _) <- get
    case findQualifiedExpr mn i of
      Nothing -> throwError (IdentifierNotFound cmn ann qi)
      Just (Right e)  -> eval e
      Just (Left _)   -> return Nothing
  eval (Literal ann (ArrayLiteral es)) = do
    es' <- traverse (\e -> fromMaybe e <$> eval e) es
    return $ Just (Literal ann (ArrayLiteral es'))
  eval (Literal ann (ObjectLiteral as)) = do
    as' <- traverse (\(n, e) -> maybe (n,e) ((n,)) <$> eval e) as
    return $ Just (Literal ann (ObjectLiteral as'))
  eval e@Literal{} = return (Just e)
  eval
    (App ann
      (App _
        (App _
          (Var _
            (Qualified
              (Just C.Eq)
              (Ident "eq")))
          (Var _ inst))
          e1)
      e2)
    = if inst `elem`
          [ Qualified (Just C.eqMod) (Ident "eqBoolean")
          , Qualified (Just C.eqMod) (Ident "eqInt")
          , Qualified (Just C.eqMod) (Ident "eqNumber")
          , Qualified (Just C.eqMod) (Ident "eqChar")
          , Qualified (Just C.eqMod) (Ident "eqString")
          , Qualified (Just C.eqMod) (Ident "eqUnit")
          , Qualified (Just C.eqMod) (Ident "eqVoid")
          ]
        then do
          v1 <- eval e1
          v2 <- eval e2
          case (v1, v2) of
            (Just (Literal _ l1), Just (Literal _ l2))
              -> return $ Just $ Literal ann $ BooleanLiteral (eqLit l1 l2)
            _ -> return Nothing
        else return Nothing
  eval (Accessor ann a (Literal _ (ObjectLiteral as))) = do
    (mn, _) <- get
    e <- maybe (throwError (AccessorNotFound mn ann a)) return (a `lookup` as)
    eval e
  eval (App _
          (App _
            (Var ann@(ss, _, _, _)
              (Qualified
                (Just (ModuleName "Data.Array"))
                (Ident "index")))
            (Literal _ (ArrayLiteral as)))
          (Literal _ (NumericLiteral (Left x))))
    = do
      (mn, _) <- get
      e <- maybe
            (tell [ArrayIdxOutOfBound mn ann x] $> Nothing)
            eval
            (as `atMay` fromIntegral x)
      return
          $ App ann
              (Var (ss, [], Nothing, Just (IsConstructor SumType [Ident "value0"]))
                (Qualified
                  (Just C.maybeMod)
                  (Ident "Just")))
        <$> e
  -- | Eval Semigroup
  eval
    (App ann
      (App _
        (App _
           (Var _ (Qualified (Just C.Semigroup) (Ident "append")))
           (Var _ qi))
        e1)
      e2)
      | qi == Qualified (Just C.semigroup) (Ident "semigroupArray")
      , Literal _ (ArrayLiteral a1) <- e1
      , Literal _ (ArrayLiteral a2) <- e2
      = return $ Just $ Literal ann (ArrayLiteral $ a1 ++ a2)
      | qi == Qualified (Just C.semigroup) (Ident "semigroupString")
      , Literal _ (StringLiteral s1) <- e1
      , Just t1 <- decodeString s1
      , Literal _ (StringLiteral s2) <- e2
      , Just t2 <- decodeString s2
      = return $ Just $ Literal ann (StringLiteral (mkString $ t1 <> t2) )
      | qi == Qualified (Just C.semigroup) (Ident "semigroupUnit")
      = return $ Just $ Var ann (Qualified (Just C.unit) (Ident "unit"))
      | otherwise
      = return Nothing
  -- | Eval Semiring
  eval
    (App (ss, c, _, _)
      (App _
        (App _
           (Var _ (Qualified (Just C.Semiring) (Ident "add")))
           (Var _ qi))
        e1)
      e2)
    | qi == Qualified (Just C.semiring) (Ident "semiringInt")
    , Literal _ (NumericLiteral (Left a1)) <- e1
    , Literal _ (NumericLiteral (Left a2)) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (a1 + a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringNumber")
    , Literal _ (NumericLiteral (Right a1)) <- e1
    , Literal _ (NumericLiteral (Right a2)) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (a1 + a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.Semiring) (Ident "zero")))
      (Var _ qi))
    | qi == Qualified (Just C.semiring) (Ident "semiringInt")
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left 0))
    | qi == Qualified
        (Just C.semiring)
        (Ident "semiringNumber")
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right 0.0))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = return $ Just  $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.Semiring) (Ident "one")))
      (Var _ qi))
    | qi == Qualified (Just C.semiring) (Ident "semiringInt")
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left 1))
    | qi == Qualified (Just C.semiring) (Ident "semiringNumber")
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right 1.0))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = return $ Just  $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (App _
        (App _
           (Var _ (Qualified (Just C.Semiring) (Ident "mul")))
           (Var _ qi))
        e1)
      e2)
    | qi == Qualified (Just C.semiring) (Ident "semiringInt")
    , Literal _ (NumericLiteral (Left a1)) <- e1
    , Literal _ (NumericLiteral (Left a2)) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (a1 * a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringNumber")
    , Literal _ (NumericLiteral (Right a1)) <- e1
    , Literal _ (NumericLiteral (Right a2)) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (a1 * a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  -- || Eval Ring
  eval
    (App (ss, c, _, _)
      (App _
        (App _
          (Var _ (Qualified (Just C.Ring) (Ident "sub")))
          (Var _ qi))
        e1)
      e2)
    | qi == Qualified (Just C.ring) (Ident "ringInt")
    , Literal _ (NumericLiteral (Left a1)) <- e1
    , Literal _ (NumericLiteral (Left a2)) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (quot a1 a2)))
    | qi == Qualified (Just C.ring) (Ident "ringNumber")
    , Literal _ (NumericLiteral (Right a1)) <- e1
    , Literal _ (NumericLiteral (Right a2)) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (a1 / a2)))
    | qi == Qualified (Just C.ring) (Ident "unitRing")
    = return $ Just  $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
  eval
    (App (ss, c, _, _)
      (App _
        (Var _ (Qualified (Just C.Ring) (Ident "negate")))
        (Var _ qi))
      e)
    | qi == Qualified (Just C.ring) (Ident "ringInt")
    , Literal _ (NumericLiteral (Left a)) <- e
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (-a)))
    | qi == Qualified (Just C.ring) (Ident "ringNumber")
    , Literal _ (NumericLiteral (Right a)) <- e
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (-a)))
    | qi == Qualified (Just C.ring) (Ident "unitRing")
    = return $ Just  $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
  -- | Eval HeytingAlgebra
  eval
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "ff")))
      (Var _ qi))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    = return $ Just $ Literal (ss, c, Nothing, Nothing) (BooleanLiteral False)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "tt")))
      (Var _ qi))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    = return $ Just $ Literal (ss, c, Nothing, Nothing) (BooleanLiteral True)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (App _
        (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "not")))
        (Var _ qi))
      e)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    , Literal _ (BooleanLiteral b) <- e
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (not b))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (App _
        (App _
           (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "implies")))
           (Var _ qi))
        e1)
      e2)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    , Literal _ (BooleanLiteral b1) <- e1
    , Literal _ (BooleanLiteral b2) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (not b1 && b2))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (App _
        (App _
           (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "disj")))
           (Var _ qi))
        e1)
      e2)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    , Literal _ (BooleanLiteral b1) <- e1
    , Literal _ (BooleanLiteral b2) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (b1 || b2))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
  eval
    (App (ss, c, _, _)
      (App _
        (App _
           (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "conj")))
           (Var _ qi))
        e1)
      e2)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    , Literal _ (BooleanLiteral b1) <- e1
    , Literal _ (BooleanLiteral b2) <- e2
    = return $ Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (b1 && b2))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = return $ Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = return Nothing
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
  findQualifiedExpr :: ModuleName -> Ident -> Maybe (Either () (Expr Ann))
  findQualifiedExpr (ModuleName mn) _
    | "Prim" : _ <- T.splitOn "." mn
    = Just (Left ())
  findQualifiedExpr (ModuleName "Data.Generic") (Ident "anyProxy") = Just (Left ())
  findQualifiedExpr mn i
      = Right <$> (mod >>= getFirst . foldMap fIdent . concatMap unBind . moduleDecls)
    <|> Left  <$> (mod >>= getFirst . foldMap ffIdent . moduleForeign)
    where
    mod :: Maybe (Module Ann)
    mod = getFirst $ foldMap (\m -> if moduleName m == mn then First (Just m) else First Nothing) mods

    fIdent :: (Ident, Expr Ann) -> First (Expr Ann)
    fIdent (i', e) | i == i'    = First (Just e)
                   | otherwise  = First Nothing

    ffIdent :: Ident -> First ()
    ffIdent i' | i == i'   = First (Just ())
               | otherwise = First Nothing
