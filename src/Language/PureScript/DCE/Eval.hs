-- |
-- Evaluation of PureScript's expressions used in dead call elimnation.
module Language.PureScript.DCE.Eval
  ( dceEval ) where

import Control.Monad
--import Control.Monad.Except
import Control.Monad.Writer
import Data.List (find)
import Data.Maybe (fromMaybe, maybeToList)
import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
--import Language.PureScript.DCE.Errors
import Language.PureScript.DCE.Utils
import Language.PureScript.Names
import Language.PureScript.PSString

import Control.Applicative ((<|>))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Monoid (First(..))
import qualified Data.Text as T
import qualified Language.PureScript.DCE.Constants as C
import Prelude.Compat hiding (mod)
import Safe (atMay)

data EvalState
  = NotYet -- ^ an expression has not yet been evaluated
  | Done   -- ^ an expression has been evaluated
  deriving (Eq, Show)

data StackT frame =
    EmptyStack
  | ConsStack !frame !(StackT frame)
  deriving (Show, Functor)

type Stack = StackT [((Ident, Expr Ann), EvalState)]

pushStack :: [(Ident, Expr Ann)]
          -> Stack
          -> Stack
pushStack frame st = map (\s -> (s, NotYet)) frame `ConsStack` st

{-
popStack :: StackT frame
         -> StackT frame
popStack EmptyStack      = error "popStack: EmptyStack"
popStack (ConsStack _ s) = s
-}

lookupStack :: Ident
            -> Stack
            -> Maybe ((Ident, Expr Ann), EvalState)
lookupStack _i EmptyStack       = Nothing
lookupStack i  (ConsStack f fs) = case find (\((i', _), _) -> i == i') f of
    Nothing -> lookupStack i fs
    Just x  -> Just x

-- Mark first found expression as evaluated to avoid infinite loops.
markDone :: Ident -> Stack -> Stack
markDone _ EmptyStack = EmptyStack
markDone i (ConsStack l ls) =
  case foldr fn ([], False) l of
    (l', True)  -> ConsStack l' ls
    (l', False) -> ConsStack l' (markDone i ls)
  where
  fn x@(a@(i', _), _) (is, done)
    | i == i'   = ((a, Done) : is, True)
    | otherwise = (x         : is, done)

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
  :: [Module Ann]
  -> [Module Ann]
dceEval mods = map evalModule mods
  where
  evalModule :: Module Ann -> Module Ann
  evalModule mod@Module{ moduleName, moduleDecls } =
    mod { moduleDecls = map (onModuleBind moduleName) moduleDecls }

  onModuleBind :: ModuleName -> Bind Ann -> Bind Ann
  onModuleBind mn (NonRec a i e) = NonRec a i (onExpr mn EmptyStack e)
  onModuleBind mn (Rec binds')    = Rec $ map (fmap $ onExpr mn (pushStack (map (\((_, i), e) -> (i, e)) binds') EmptyStack)) binds'

  -- Push identifiers defined in binders onto the stack
  onBinders
    :: [Expr Ann]
    -> [Binder Ann]
    -> Stack
    -> Stack
  onBinders es bs = pushStack (concatMap fn (zip bs es))
    where
      fn :: (Binder Ann, Expr Ann) -> [(Ident, Expr Ann)]
      fn (NullBinder _, _ )              = []
      fn (LiteralBinder _ _, _)          = []
      fn (VarBinder _ i, e)              = [(i,e)]
      fn (ConstructorBinder _ _ _ as, e) = concatMap fn (zip as (repeat e))
      fn (NamedBinder _ i b, e)          = (i, e) : fn (b, e)

  -- |
  -- Evaluate expressions, keep the stack of local identifiers. It does not
  -- track identifiers which are coming from abstractions, but `Let` and
  -- `Case` binders are pushed into / poped from the stack.
  -- * `Let` binds are added in `onBind` and poped from the stack
  --   when visiting `Let` expression.
  -- * `Case` binds are added in `onBinders` and poped in the
  --  `everywhereOnValuesM` monadic action.
  onExpr :: ModuleName
         -> Stack
         -> Expr Ann
         -> Expr Ann
  onExpr mn st c@(Case ann es cs) =
      let es'  = map (\e -> eval mn st e >>= getLiteral) es
          cs'  = getFirst $ foldMap (fndCase ((snd . fromJust) `map` es')) cs
      in if all isJust es'
          then case cs' of
            Nothing
              -> c
            Just (CaseAlternative bs (Right e))
              | not (any binds bs)
              -> onExpr mn (pushStack [] st) e
              | otherwise
              -> Case ann es (maybeToList cs')
            Just (CaseAlternative bs (Left gs))
              -> Case ann es [CaseAlternative bs (Left (fltGuards (onBinders es bs st) gs))]
          else Case ann es $
                filter
                  (fltBinders (fmap snd `map` es') . caseAlternativeBinders)
                  cs
    where
      fltGuards
        :: Stack
        -> [(Guard Ann, Expr Ann)]
        -> [(Guard Ann, Expr Ann)]
      fltGuards _  [] = []
      fltGuards st' (guard'@(g, e) : rest) =
        case eval mn st' g of
          Just (Literal _ t)
            | t `eqLit` BooleanLiteral True
            ->  [(Literal (extractAnn g) (BooleanLiteral True), e)]
            | otherwise -- guard expression must evaluate to a Boolean
            -> fltGuards st' rest
          _ -> guard' : fltGuards st' rest

  onExpr mn st (Let _ann bs e) = onExpr mn (pushStack (concatMap unBind bs) st) e

  onExpr mn st e@Var{} =
    case eval mn st e of
      Just l@(Literal _ NumericLiteral{}) -> l
      Just l@(Literal _ CharLiteral{})    -> l
      Just l@(Literal _ BooleanLiteral{}) -> l
      -- preserve string, array and object literals
      Just _  -> e
      Nothing -> e

  onExpr mn st e =
    case eval mn st e of
      Just l  -> l
      Nothing -> e


  -- |
  -- Evaluate an expression
  -- * `Data.Eq.eq` of two literals
  -- * `Data.Array.index` on a literal array
  -- * Object accessors
  -- * Semigroup operations (Array, String, Unit)
  -- * Semiring operations (Int, Number, Unit)
  -- * Heyting algebra operations (Boolean, Unit)
  eval :: ModuleName
       -> Stack
       -> Expr Ann
       -> Maybe (Expr Ann)

  eval mn st (Var _ (Qualified Nothing i)) = 
    case lookupStack i st of
      Nothing               -> Nothing
      Just ((_, e), Done)   -> Just e
      Just ((_, e), NotYet) -> eval mn (markDone i st) e

  eval mn st (Var _ann (Qualified (Just imn) i)) =
    case findQualifiedExpr imn i of
      Nothing -> error "eval: identifier not found" -- throwError (IdentifierNotFound cmn ann qi)
      Just (Right e)  -> eval mn st e
      Just (Left _)   -> Nothing

  eval mn st (Literal ann (ArrayLiteral es)) =
    let es' = map (\e -> fromMaybe e $ eval mn st e) es
    in Just (Literal ann (ArrayLiteral es'))

  eval mn st (Literal ann (ObjectLiteral as)) =
    let as' = map (\x@(n, e) -> case eval mn st e of
                                  Nothing -> x
                                  Just e' -> (n, e')
                  ) as
    in Just (Literal ann (ObjectLiteral as'))

  eval _mn _st e@Literal{} = Just e

  eval mn st (Accessor _ann a (Literal _ (ObjectLiteral as))) =
    case a `lookup` as of
      -- TODO: use Either to return error
      Nothing -> error "accessor not found" -- throwError (AccessorNotFound mn ann a)
      Just e  -> eval mn st e

  --
  -- evaluate boolean operations
  --
  eval mn st
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
        then case (eval mn st e1, eval mn st e2) of
              (Just (Literal _ l1), Just (Literal _ l2))
                -> Just $ Literal ann $ BooleanLiteral (eqLit l1 l2)
              _ -> Nothing
        else Nothing

  --
  -- evaluate array indexing
  --
  eval mn st
        (App _
          (App _
            (Var ann@(ss, _, _, _)
              (Qualified
                (Just (ModuleName "Data.Array"))
                (Ident "index")))
            (Literal _ (ArrayLiteral as)))
          (Literal _ (NumericLiteral (Left x))))
    = case (as `atMay` fromIntegral x) of
        Nothing -> error "ArrayIdxOutOfBound"
        Just e -> case  eval mn st e of
          Nothing -> Nothing
          Just e' ->
            Just $ App ann
                    (Var (ss, [], Nothing, Just (IsConstructor SumType [Ident "value0"]))
                      (Qualified
                        (Just C.maybeMod)
                        (Ident "Just")))
                    e'
  --
  -- evalualte semigroup operations
  --
  eval _ms _st
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
      = Just $ Literal ann (ArrayLiteral $ a1 ++ a2)
      | qi == Qualified (Just C.semigroup) (Ident "semigroupString")
      , Literal _ (StringLiteral s1) <- e1
      , Just t1 <- decodeString s1
      , Literal _ (StringLiteral s2) <- e2
      , Just t2 <- decodeString s2
      = Just $ Literal ann (StringLiteral (mkString $ t1 <> t2) )
      | qi == Qualified (Just C.semigroup) (Ident "semigroupUnit")
      = Just $ Var ann (Qualified (Just C.unit) (Ident "unit"))
      | otherwise
      = Nothing
  --
  -- evalulate semiring operations
  --
  eval _mn _st
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
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (a1 + a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringNumber")
    , Literal _ (NumericLiteral (Right a1)) <- e1
    , Literal _ (NumericLiteral (Right a2)) <- e2
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (a1 + a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.Semiring) (Ident "zero")))
      (Var _ qi))
    | qi == Qualified (Just C.semiring) (Ident "semiringInt")
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left 0))
    | qi == Qualified
        (Just C.semiring)
        (Ident "semiringNumber")
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right 0.0))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = Just  $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.Semiring) (Ident "one")))
      (Var _ qi))
    | qi == Qualified (Just C.semiring) (Ident "semiringInt")
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left 1))
    | qi == Qualified (Just C.semiring) (Ident "semiringNumber")
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right 1.0))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = Just  $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
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
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (a1 * a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringNumber")
    , Literal _ (NumericLiteral (Right a1)) <- e1
    , Literal _ (NumericLiteral (Right a2)) <- e2
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (a1 * a2)))
    | qi == Qualified (Just C.semiring) (Ident "semiringUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  --
  -- evaluate ring operations
  --
  eval _mn _st
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
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (quot a1 a2)))
    | qi == Qualified (Just C.ring) (Ident "ringNumber")
    , Literal _ (NumericLiteral (Right a1)) <- e1
    , Literal _ (NumericLiteral (Right a2)) <- e2
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (a1 / a2)))
    | qi == Qualified (Just C.ring) (Ident "unitRing")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
  eval _mn _st
    (App (ss, c, _, _)
      (App _
        (Var _ (Qualified (Just C.Ring) (Ident "negate")))
        (Var _ qi))
      e)
    | qi == Qualified (Just C.ring) (Ident "ringInt")
    , Literal _ (NumericLiteral (Left a)) <- e
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Left (-a)))
    | qi == Qualified (Just C.ring) (Ident "ringNumber")
    , Literal _ (NumericLiteral (Right a)) <- e
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (NumericLiteral (Right (-a)))
    | qi == Qualified (Just C.ring) (Ident "unitRing")
    = Just  $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
  --
  -- evaluate Heyting algebras operations
  --
  eval _mn _st
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "ff")))
      (Var _ qi))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    = Just $ Literal (ss, c, Nothing, Nothing) (BooleanLiteral False)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
    (App (ss, c, _, _)
      (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "tt")))
      (Var _ qi))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    = Just $ Literal (ss, c, Nothing, Nothing) (BooleanLiteral True)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
    (App (ss, c, _, _)
      (App _
        (Var _ (Qualified (Just C.HeytingAlgebra) (Ident "not")))
        (Var _ qi))
      e)
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraBoolean")
    , Literal _ (BooleanLiteral b) <- e
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (not b))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
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
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (not b1 && b2))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
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
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (b1 || b2))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing
  eval _mn _st
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
    = Just $ Literal
        (ss, c, Nothing, Nothing)
        (BooleanLiteral (b1 && b2))
    | qi == Qualified (Just C.heytingAlgebra) (Ident "heytingAlgebraUnit")
    = Just $ Var
        (ss, c, Nothing, Nothing)
        (Qualified (Just C.unit) (Ident "unit"))
    | otherwise
    = Nothing

  --
  -- default case (no evaluation)
  --
  eval _ _ _ = Nothing

  eqLit :: Literal a -> Literal b -> Bool
  eqLit (NumericLiteral (Left a))  (NumericLiteral (Left b))  = a == b
  eqLit (NumericLiteral (Right a)) (NumericLiteral (Right b)) = a == b
  eqLit (StringLiteral a)          (StringLiteral b)          = a == b
  eqLit (CharLiteral a)            (CharLiteral b)            = a == b
  eqLit (BooleanLiteral a)         (BooleanLiteral b)         = a == b
  eqLit _                          _                          = False

  fltBinders :: [Maybe (Literal (Expr Ann))]
             -> [Binder Ann]
             -> Bool
  fltBinders (Just l1 : ts) (LiteralBinder _ l2 : bs) = l1 `eqLit` l2 && fltBinders ts bs
  fltBinders _              _                         = True

  getLiteral :: Expr Ann -> Maybe (Ann, Literal (Expr Ann))
  getLiteral (Literal ann l) = Just (ann, l)
  getLiteral _               = Nothing

  fndCase :: [Literal (Expr Ann)] -> CaseAlternative Ann -> First (CaseAlternative Ann)
  fndCase as c =
      if matches as (caseAlternativeBinders c)
        then First (Just c)
        else First Nothing
    where
      matches :: [Literal (Expr Ann)] -> [Binder Ann] -> Bool
      matches [] [] = True
      matches [] _  = error "fndCase: not maching case expressions and case alternatives"
      matches _ []  = error "fndCase: not maching case expressions and case alternatives"
      matches (t:ts) (LiteralBinder _ t' : bs) = t `eqLit` t' && matches ts bs
      matches (t:ts) (NamedBinder _ _ (LiteralBinder _ t') : bs) = t `eqLit` t' && matches ts bs
      matches (_:ts) (_:bs) = matches ts bs

  -- Does a binder binds?
  binds :: Binder Ann -> Bool
  binds (NullBinder _)                       = False
  binds (LiteralBinder _ (NumericLiteral _)) = False
  binds (LiteralBinder _ (StringLiteral _))  = False
  binds (LiteralBinder _ (CharLiteral _))    = False
  binds (LiteralBinder _ (BooleanLiteral _)) = False
  binds (LiteralBinder _ (ArrayLiteral bs))  = any binds bs
  binds (LiteralBinder _ (ObjectLiteral bs)) = any (binds . snd) bs
  binds (VarBinder _ _)                      = True
  binds (ConstructorBinder _ _ _ bs)         = any binds bs
  binds NamedBinder{}                        = True

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
