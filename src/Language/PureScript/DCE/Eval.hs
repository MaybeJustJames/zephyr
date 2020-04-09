-- | Evaluation of PureScript's expressions used in dead call elimnation.
--
module Language.PureScript.DCE.Eval
  ( evaluate ) where

import Control.Applicative ((<|>))
import Control.Exception (Exception (..), throw)
import Control.Monad
import Control.Monad.Writer

import Data.List (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (First(..))
import qualified Data.Text as T
import qualified Language.PureScript.DCE.Constants as C
import Prelude.Compat hiding (mod)
import Safe (atMay)

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn
--import Language.PureScript.DCE.Errors
import Language.PureScript.DCE.Utils
import Language.PureScript.Names
import Language.PureScript.PSString


data EvalState
  = NotYet -- ^ an expression has not yet been evaluated
  | Done   -- ^ an expression has been evaluated
  deriving (Eq, Show)


data StackT frame =
    EmptyStack
  | ConsStack !frame !(StackT frame)
  deriving (Show, Functor)


type Stack = StackT [((Ident, Expr Ann), EvalState)]

-- | Errors thrown by the evaluation.
--
data EvaluationError
    = QualifiedExpresionError Ann (Qualified Ident) ![ModuleName]
    -- ^ qualified expression not found in the list of modules
    | OutOfBoundArrayIndex Ann
    -- ^ out of bound array index
    | NotFoundRecordField Ann PSString
    -- ^ record field not found
  deriving Show

instance Exception EvaluationError


pushStack :: [(Ident, Expr Ann)]
          -> Stack
          -> Stack
pushStack frame st = map (\s -> (s, NotYet)) frame `ConsStack` st


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


-- | Evaluate expressions in a module:
--
-- * @Data.Eq.eq@ of two literals
-- * @Data.Array.index@ on a literal array
-- * Object accessors
-- * Semigroup operations (@Array@, @String@, @Unit@)
-- * Semiring operations (@Unit@, @Unit@, @Unit@)
--
-- Keep stack of local identifiers from @let@ and @case@ expressions, ignoring
-- the ones that are comming from abstractions (we are not reducing
-- applications).
--
evaluate :: [Module Ann] -> [Module Ann]

evaluate mods = rewriteModule `map` mods
  where

    rewriteModule :: Module Ann -> Module Ann
    rewriteModule mod@Module{ moduleName, moduleDecls } =
      mod { moduleDecls = rewriteBind moduleName `map` moduleDecls }


    rewriteBind :: ModuleName
                -> Bind Ann -> Bind Ann
    rewriteBind mn (NonRec a i e) =
      NonRec a i (rewriteExpr mn EmptyStack e)

    rewriteBind mn (Rec binds')   =
      Rec [ rewriteExpr mn stack <$> bind'
          | bind' <- binds' 
          ]
        where
          stack = pushStack ((\((_, i), e) -> (i, e)) `map` binds')
                            EmptyStack


    -- Push identifiers defined in binders onto the stack
    pushBinders :: [Expr Ann] -> [Binder Ann] -> Stack -> Stack
    pushBinders es bs = pushStack (concatMap fn (zip bs es))
      where
        fn :: (Binder Ann, Expr Ann) -> [(Ident, Expr Ann)]
        fn (NullBinder _, _ )              = []
        fn (LiteralBinder _ _, _)          = []
        fn (VarBinder _ i, e)              = [(i,e)]
        fn (ConstructorBinder _ _ _ as, e) = concatMap fn (zip as (repeat e))
        fn (NamedBinder _ i b, e)          = (i, e) : fn (b, e)

    -- | Evaluate expressions, keep the stack of local identifiers. It does not
    -- track identifiers which are coming from abstractions, but `Let` and
    -- `Case` binders are pushed into / poped from the stack.
    --
    -- * `Let` binds are added in `onBind` and poped from the stack
    --   when visiting `Let` expression.
    -- * `Case` binds are added in `pushBinders` and poped in the
    --  `everywhereOnValuesM` monadic action.
    --
    rewriteExpr :: ModuleName -> Stack
                -> Expr Ann -> Expr Ann
    rewriteExpr mn st c@(Case ann es cs) =
        -- purescript is a strict language, so we can take advantage of that
        -- and evalute all the expressions now
        let es' :: [Maybe (Expr Ann)]
            es' = eval mods mn st `map` es
        in case traverse (join . fmap fltLiteral) es' of
          Nothing ->
            -- remove cases whcich do not match
            Case ann es
              $ filter
                  (fltBinders ((>>= fltLiteral) `map` es') . caseAlternativeBinders)
                  cs
          Just es'' ->
            -- all es evaluated to a literal, we can try to find the matching
            -- `CaseAlternative`
            case foldMap (fndCase es'') cs of
              First Nothing -> c
              First (Just (CaseAlternative bs (Right e)))
                -- we found a matching `CaseAlternative`, we can eliminate the case
                -- expression
                -> rewriteExpr mn (pushBinders es'' bs st) e
              First (Just (CaseAlternative bs (Left gs)))
                -- we found a matching `CaseAlternative` with guards; we can
                -- simplify the case expression and the list of guards
                -> Case ann es [CaseAlternative bs (Left (fltGuards mn (pushBinders es bs st) gs))]

    rewriteExpr mn st (Let _ann bs e) = rewriteExpr mn (pushStack (concatMap unBind bs) st) e

    rewriteExpr mn st e@Var{} =
      case eval mods mn st e of
        Just l@(Literal _ NumericLiteral{}) -> l
        Just l@(Literal _ CharLiteral{})    -> l
        Just l@(Literal _ BooleanLiteral{}) -> l
        -- preserve string, array and object literals
        Just _  -> e
        Nothing -> e

    rewriteExpr mn st e =
      case eval mods mn st e of
        Just l  -> l
        Nothing -> e

    fltBinders :: [Maybe (Expr Ann)]
               -> [Binder Ann]
               -> Bool
    fltBinders (Just (Literal _ l1) : ts) (LiteralBinder _ l2 : bs) =
      l1 `eqLit` l2 && fltBinders ts bs
    fltBinders _ _ = True

    fltGuards
      :: ModuleName
      -> Stack
      -> [(Guard Ann, Expr Ann)]
      -> [(Guard Ann, Expr Ann)]
    fltGuards _ _  [] = []
    fltGuards mn st (guard'@(g, e) : rest) =
      case eval mods mn st g of
        Just (Literal _ t)
          | t `eqLit` BooleanLiteral True
          ->  [(Literal (extractAnn g) (BooleanLiteral True), e)]
          | otherwise -- guard expression must evaluate to a Boolean
          -> fltGuards mn st rest
        _ -> guard' : fltGuards mn st rest

    fltLiteral :: Expr Ann -> Maybe (Expr Ann)
    fltLiteral e@Literal {} = Just e
    fltLiteral _            = Nothing

    -- match a list of literal expressions against a case alternative
    fndCase :: [Expr Ann] -> CaseAlternative Ann -> First (CaseAlternative Ann)
    fndCase as c =
        if as `matches` caseAlternativeBinders c
          then First (Just c)
          else First Nothing
      where
        matches :: [Expr Ann] -> [Binder Ann] -> Bool
        matches [] [] = True
        matches [] _  = error "impossible happend: not matching case expressions and case alternatives"
        matches _ []  = error "impossible happend: not matching case expressions and case alternatives"
        matches (Literal _ t:ts) (LiteralBinder _ t' : bs) = t `eqLit` t' && matches ts bs
        matches (Literal _ t:ts) (NamedBinder _ _ (LiteralBinder _ t') : bs) = t `eqLit` t' && matches ts bs
        matches (Literal {}:ts) (_:bs) = matches ts bs
        matches (_:_) (_:_) = False


-- | Evaluate an expresion
--
-- * `Data.Eq.eq` of two literals
-- * `Data.Array.index` on a literal array
-- * Object accessors
-- * Semigroup operations (Array, String, Unit)
-- * Semiring operations (Int, Number, Unit)
-- * Heyting algebra operations (Boolean, Unit)
--
eval :: [Module Ann]
     -> ModuleName
     -> Stack
     -> Expr Ann
     -> Maybe (Expr Ann)

eval mods mn st (Var _ (Qualified Nothing i)) = 
    case lookupStack i st of
      Nothing               -> Nothing
      Just ((_, e), Done)   -> Just e
      Just ((_, e), NotYet) -> eval mods mn (markDone i st) e

eval mods mn st (Var ann qi@(Qualified (Just imn) i)) =
    case lookupQualifiedExpr mods imn i of
      Nothing             -> throw (QualifiedExpresionError ann qi (moduleName `map` mods))
      Just (FoundExpr e)  -> eval mods mn st e
      Just Found          -> Nothing

eval mods mn st (Literal ann (ArrayLiteral es)) =
    let es' = map (\e -> fromMaybe e $ eval mods mn st e) es
    in Just (Literal ann (ArrayLiteral es'))

eval mods mn st (Literal ann (ObjectLiteral as)) =
    let as' = map (\x@(n, e) ->
                    case eval mods mn st e of
                      Nothing -> x
                      Just e' -> (n, e')
                  ) as
    in Just (Literal ann (ObjectLiteral as'))

eval _mods _mn _st e@Literal{} = Just e

eval mods mn st (Accessor ann a (Literal _ (ObjectLiteral as))) =
    case a `lookup` as of
      -- this cannot happen, unless an unsafe usage of ffi
      Nothing -> throw (NotFoundRecordField ann a)
      Just e  -> eval mods mn st e

--
-- evaluate boolean operations
--
eval mods mn st
    (App ann
      (App _
        (App _
          (Var _
            (Qualified
              (Just C.Eq)
              (Ident "eq")))
          (Var _ inst))
          e1)
      e2) =
    if inst `elem`
          [ Qualified (Just C.eqMod) (Ident "eqBoolean")
          , Qualified (Just C.eqMod) (Ident "eqInt")
          , Qualified (Just C.eqMod) (Ident "eqNumber")
          , Qualified (Just C.eqMod) (Ident "eqChar")
          , Qualified (Just C.eqMod) (Ident "eqString")
          , Qualified (Just C.eqMod) (Ident "eqUnit")
          , Qualified (Just C.eqMod) (Ident "eqVoid")
          ]
      then case (eval mods mn st e1, eval mods mn st e2) of
          (Just (Literal _ l1), Just (Literal _ l2))
            -> Just $ Literal ann $ BooleanLiteral (eqLit l1 l2)
          _ -> Nothing
      else Nothing

--
-- evaluate array indexing
--
eval mods mn st
      (App _
        (App _
          (Var ann@(ss, _, _, _)
            (Qualified
              (Just (ModuleName "Data.Array"))
              (Ident "index")))
          (Literal _ (ArrayLiteral as)))
        (Literal _ (NumericLiteral (Left x)))) =
    case (as `atMay` fromIntegral x) of
      Nothing -> throw (OutOfBoundArrayIndex ann)
      Just e -> case  eval mods mn st e of
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
eval _ _ms _st
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
eval _ _mn _st
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

eval _ _mn _st
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

eval _ _mn _st
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

eval _ _mn _st
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
eval _ _mn _st
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

eval _ _mn _st
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
eval _ _mn _st
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

eval _ _mn _st
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

eval _mods _mn _st
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

eval _mods _mn _st
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

eval _mods _mn _st
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

eval _mods _mn _st
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
eval _ _ _ _ = Nothing


-- | Lookup result, either with or without the evidence.
--
data LookupResult =
      FoundExpr !(Expr Ann)
    | Found


-- | Find a qualified name in the list of modules `mods`, return `Found` for
-- `Prim` values, generics and foreign imports, `Right` for found bindings.
--
lookupQualifiedExpr :: [Module Ann]
                    -> ModuleName
                    -> Ident
                    -> Maybe LookupResult
lookupQualifiedExpr _ (ModuleName mn) _
    | "Prim" : _ <- T.splitOn "." mn
    = Just Found
lookupQualifiedExpr _ (ModuleName "Data.Generic") (Ident "anyProxy") =
    Just Found
lookupQualifiedExpr mods mn i =
        (mod >>= fmap FoundExpr
               . lookup i
               . concatMap unBind
               . moduleDecls)
    <|> (mod >>= fmap (const Found)
               . find (== i)
               . moduleForeign)
  where
    mod = find (\m -> moduleName m == mn) mods


eqLit :: Literal a -> Literal b -> Bool
eqLit (NumericLiteral (Left a))  (NumericLiteral (Left b))  = a == b
eqLit (NumericLiteral (Right a)) (NumericLiteral (Right b)) = a == b
eqLit (StringLiteral a)          (StringLiteral b)          = a == b
eqLit (CharLiteral a)            (CharLiteral b)            = a == b
eqLit (BooleanLiteral a)         (BooleanLiteral b)         = a == b
eqLit _                          _                          = False
