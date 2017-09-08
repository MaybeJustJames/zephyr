-- Tree shaking of type classes instance members on CoreFn
module Language.PureScript.DCE.Instances
  ( dceInstances ) where

import           Prelude.Compat
import           Control.Arrow ((&&&), first)
import           Control.Applicative ((<|>))
import           Control.Comonad.Cofree
import           Control.Monad
import           Control.Monad.State
import           Data.Graph
import           Data.List (any, elem, filter, groupBy, sortBy)
import qualified Data.Map.Strict as M
import           Data.Maybe (Maybe(..), catMaybes, fromMaybe, mapMaybe)
import           Data.Monoid (Alt(Alt), getAlt)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Constants as C
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.PSString (PSString, decodeString, mkString)

import           Language.PureScript.DCE.Utils

type ModuleDict = M.Map ModuleName (ModuleT () Ann)

-- |
-- Information gethered from `CoreFn.Meta.IsTypeClassConstructor`
type TypeClassDict = M.Map (Qualified (ProperName 'ClassName)) [(PSString, Maybe (Qualified (ProperName 'ClassName)))]

buildTypeClassDict :: forall t. [ModuleT t Ann] -> TypeClassDict
buildTypeClassDict mods = execState (sequence_ [onModule m | m <- mods])  M.empty
  where
  onModule (Module _ mn _ _ _ decls) = sequence_ [ onDecl mn decl | decl <- decls ]

  onDecl :: ModuleName -> Bind Ann -> State TypeClassDict ()
  onDecl mn (NonRec _ i e) = onExpr (mkQualified (identToProper i) mn) e
  onDecl mn (Rec bs) = mapM_ (\((_, i), e) -> onExpr (mkQualified (identToProper i) mn) e) bs

  onExpr :: Qualified (ProperName 'ClassName) -> Expr Ann -> State TypeClassDict ()
  onExpr ident (Abs (_, _, _, Just (IsTypeClassConstructor mbs)) _ _)
    = modify (M.insert ident (first mkString `map` mbs))
  onExpr _ _ = return ()

data InstanceData = InstanceData
  { instTypeClass :: Qualified (ProperName 'ClassName)
  , instExpr :: Expr Ann
  }
  deriving (Show)
type InstancesDict = M.Map (Qualified Ident) InstanceData
-- ^
-- Dictionary of all instances accross all modules.
--
-- It allows to efficiently check if an identifier used in an expression is an
-- instance declaration.

isInstance :: Qualified Ident -> InstancesDict -> Bool
isInstance (Qualified Nothing _) _ = False
isInstance ident dict = ident `M.member` dict

buildInstancesDict :: [ModuleT t Ann] -> InstancesDict
buildInstancesDict mods = M.fromList (instancesInModule `concatMap` mods)
  where
  instancesInModule :: ModuleT t Ann -> [(Qualified Ident, InstanceData)]
  instancesInModule =
      concatMap instanceDicts
    . uncurry zip
    . first repeat
    . (moduleName &&& moduleDecls)

  instanceDicts :: (ModuleName, Bind Ann) -> [(Qualified Ident, InstanceData)]
  instanceDicts (mn, NonRec _ i e)  | Just tyClsName <- isInstanceOf e = [(mkQualified i mn, InstanceData tyClsName e)]
                                    | otherwise = []
  instanceDicts (mn, Rec bs) = mapMaybe
    (\((_, i), e) ->
      case isInstanceOf e of
        Just tyClsName  -> Just (mkQualified i mn, InstanceData tyClsName e)
        Nothing         -> Nothing)
    bs

data TypeClassInstDepsData = TypeClassInstDepsData
  { tciClassName :: Qualified (ProperName 'ClassName)
  , tciName :: PSString
  }
  deriving (Show)
type TypeClassInstDeps = Cofree Maybe TypeClassInstDepsData
-- ^
-- Tree structure that encodes information about type
-- class instance dependencies for constrained types.  Each constraint will
-- map to a list of `TypeClassInstanceDeps` (each call to a memeber will
-- correspond to one `TypeClassInstDeps`).
--
-- `tciClassName` is the _TypeClass_ name
-- `tciName` is the field name of a member or parent type class used in
-- generated code.  This information is available in
-- `Language.PureScript.CoreFn.Meta` (see
-- [corefn-typeclasses](https://github.com/coot/purescript/blob/corefn-typeclasses/src/Language/PureScript/CoreFn/Meta.hs#L29)
-- branch of my `purescript` repo clone).

type MemberAccessorDict = M.Map (Qualified Ident) TypeClassInstDepsData
-- ^ Dictionary of all instance method functions, e.g.
-- ```
-- Control.Applicative.apply
-- ```
-- will correspond to
-- ```
-- ```
-- ("apply", Control.Applicative.Applicative)
-- ```

-- |
-- PureScript generates thes functions that access members of type class
-- dictionaries.  This checks if an expression is such an abstraction.
isMemberAccessor :: TypeClassDict -> Expr Ann -> Maybe TypeClassInstDepsData
isMemberAccessor tyd (Abs (_, _, Just ty, _) ident (Accessor _ acc (Var _ (Qualified Nothing ident'))))
  | Just c <- mConstraint
  , ident == ident'
  -- check that the constraintClass has the given member
  , Just True <- elem (acc, Nothing) <$> (P.constraintClass c `M.lookup` tyd)
    = Just $ TypeClassInstDepsData (P.constraintClass c) acc
  where
    mConstraint :: Maybe P.Constraint
    mConstraint = getAlt $ P.everythingOnTypes (<|>) go ty
      where
      go (P.ConstrainedType c _) = Alt (Just c)
      go _ = Alt Nothing
isMemberAccessor _ _ = Nothing

buildMemberAccessorDict :: TypeClassDict -> [ModuleT t Ann] -> MemberAccessorDict
buildMemberAccessorDict typeClassDict mods = execState (sequence_ [onModule m | m <- mods]) M.empty
  where
  onModule (Module _ mn _ _ _ decls) = sequence_ [ onDecl mn decl | decl <- decls ]

  onDecl :: ModuleName -> Bind Ann -> State MemberAccessorDict ()
  onDecl mn (NonRec _ i e) = onExpr (mkQualified i mn) e
  onDecl mn (Rec bs) = mapM_ (\((_, i), e) -> onExpr (mkQualified i mn) e) bs

  onExpr :: Qualified Ident -> Expr Ann -> State MemberAccessorDict ()
  onExpr i e | Just x <- isMemberAccessor typeClassDict e = modify (M.insert i x)
             | otherwise                    = pure ()

dceInstances :: forall t. [ModuleT t Ann] -> [ModuleT t Ann]
dceInstances mods = undefined
  where
  instancesDict :: InstancesDict
  instancesDict = buildInstancesDict mods

  typeClassDict :: TypeClassDict
  typeClassDict = buildTypeClassDict mods

  memberAccessorDict :: MemberAccessorDict
  memberAccessorDict = buildMemberAccessorDict typeClassDict mods

-- | returns type class instance of an instance declaration
isInstanceOf :: Expr Ann -> Maybe (Qualified (ProperName 'ClassName))
isInstanceOf = getAlt . go
  where
  (_, go, _, _) = everythingOnValues (<|>) (const (Alt Nothing)) isClassConstructorOf (const (Alt Nothing)) (const (Alt Nothing))

  isClassConstructorOf :: Expr Ann -> Alt Maybe (Qualified (ProperName 'ClassName))
  isClassConstructorOf (Var (_, _, _, Just (IsTypeClassConstructorApp cn)) _) = Alt (Just cn)
  isClassConstructorOf _ = Alt Nothing

-- |
-- Get all type class names for a constrained type.
typeClassNames :: P.Type -> [Qualified (ProperName 'ClassName)]
typeClassNames (P.ForAll _ ty _) = typeClassNames ty
typeClassNames (P.ConstrainedType c ty) = P.constraintClass c : typeClassNames ty
typeClassNames _ = []

-- |
-- Get all instance names used by an expression.
exprInstances :: InstancesDict -> Expr Ann -> [Qualified Ident]
exprInstances d = go
  where
  (_, go, _, _) = everythingOnValues (++) (const []) onExpr (const []) (const [])

  onExpr :: Expr Ann -> [Qualified Ident]
  onExpr (Var _ i) | i `isInstance` d = [i]
  onExpr _ = []

-- |
-- Find all instance dependencies of an expression with a constrained type
-- The identifier is the argument name used passed to access the instances,
-- e.g. `dictMonad` or `Data.Maybe.maybeMonad`.
exprInstDeps
  :: TypeClassDict
  -> MemberAccessorDict
  -> Expr Ann
  -> [(Qualified Ident, TypeClassInstDeps)]
exprInstDeps tcDict maDict expr = execState (everywhereOnAppM_ onApp expr) []
  where
  onApp :: Ann -> Expr Ann -> Expr Ann -> State [(Qualified Ident, TypeClassInstDeps)] ()
  onApp (_, _, Just ty, _) (Var _ i) arg
    | Just (P.Constraint tcn _ _) <- getConstraint ty
    , Just d <- i `M.lookup` maDict
    = modify ( (buildTCDeps tcn d arg) : )
    | otherwise
    = return ()

  -- |
  -- Recursive routine which builds dependency instance tree
  -- starting with a type class name and the final member accessor.
  --
  -- PureScript calls member accessor function with apropriate
  -- dictionary, from that call we know the final type class and its member,
  -- here we scan the tree to build the path from the type class that
  -- constraints this member accessor function to this final type class.
  --
  -- [ref](https://hackage.haskell.org/package/purescript-0.11.6/docs/src/Language-PureScript-Sugar-TypeClasses.html#desugarDecl)
  buildTCDeps
      :: Qualified (ProperName 'ClassName)
      -- ^ initial type class name
      -> TypeClassInstDepsData
      -- ^ final TypeClassInstDepsData that is available from a member accessor
      -- call that starts the AST tree that we are analyzing.
      -> Expr Ann
      -> (Qualified Ident, TypeClassInstDeps)
  -- ```
  -- App
  --   (Var Data.Show.show)
  --   (Var Data.Show.showInt)
  -- ```
  buildTCDeps _ tcidd (Var _ i) = (i, tcidd :< Nothing)
  -- ```
  -- class A a where
  --   memeber :: a -> Int
  --
  -- class A a <= B a
  -- class B a <= C a
  --
  -- mem :: forall a. C a => a -> Int
  -- mem = member
  -- App
  --    (Var Main.a)
  --    (App
  --      (Accessor B0
  --        (App
  --          (Accessor A0
    --          (Var dictA))
  --          (Var Prim.undefined)))
  --      (Var Prim.undefined)
  -- ```
  buildTCDeps tcn tcidd
      (App _ (Accessor _ accessor e)
      (Var _ (Qualified (Just C.Prim) (Ident "undefined"))))
      | Just ptcn <- superTCName tcn accessor
      = let (i, tail) = buildTCDeps ptcn tcidd e
        in (i, TypeClassInstDepsData tcn accessor :< Just tail)
  buildTCDeps _ _ _ = undefined -- should be an error

  superTCName :: Qualified (ProperName 'ClassName) -> PSString -> Maybe (Qualified (ProperName 'ClassName))
  superTCName tcn accessor
    | Just mbrs <- tcn `M.lookup` tcDict
    = join $ getAlt $ foldMap (\(s, ptcn) -> if s == accessor then Alt (Just ptcn) else Alt Nothing) mbrs
    | otherwise
    = Nothing

-- |
-- For a given _constrained_ expression, we need to find out all the instances
-- that are used.  For each set of them we pair them with the corresponsing
-- `TypeClassInstDeps` and compute all memeber that are used.
compDeps :: [(InstanceData, TypeClassInstDeps)] -> [(Qualified Ident, [Ident])]
compDeps = undefined

-- |
-- Find all instance dependencies of a class member instance.
-- The result is an instance name with the list of all members of its class
-- that are used.
--
-- This is much simpler that `exprInstDeps` since in member declarations,
-- instances are mentioned directly.
memberDeps :: TypeClassDict -> MemberAccessorDict -> Expr Ann -> M.Map (Qualified Ident) [PSString]
memberDeps tcDict maDict expr = execState (onExpr expr) M.empty
  where

  onExpr :: Expr Ann -> State (M.Map (Qualified Ident) [PSString]) ()
  onExpr app@App{} = do
    let (f, args) = unApp app
    case f of
      (Var (_, _, _, Just (IsTypeClassConstructorApp cn)) _) ->
        case cn `M.lookup` tcDict of
          -- this should error
          Nothing   -> return ()
          -- Under assumption that CoreFn and the information in
          -- `IsTypeClassConstructor` are in the same order
          Just mbs  -> mapM_ fn (zip mbs args)
      _ -> return ()

    where
    fn :: ((PSString, Maybe (Qualified (ProperName 'ClassName))), Expr Ann) -> State (M.Map (Qualified Ident) [PSString]) ()
    fn ((acc, Just _), _) = return ()
    fn ((acc, Nothing), e) = onApp e
      where
      updateState :: Qualified Ident -> PSString -> State (M.Map (Qualified Ident) [PSString]) ()
      updateState i acc = modify (M.alter (Just . (acc :) . fromMaybe []) i)

      onApp :: Expr Ann -> State (M.Map (Qualified Ident) [PSString]) ()
      onApp (Accessor _ _ e) = onApp e
      onApp (ObjectUpdate _ e es) = onApp e *> mapM_ (onApp . snd) es
      onApp (Abs _ _ e) = onApp e
      onApp app@App{}
        | (Var _ accMemberF, Var _ instName : args) <- unApp app
        = do
            case accMemberF `M.lookup` maDict of
                Nothing -> return ()
                Just (TypeClassInstDepsData _ acc) -> updateState instName acc
            mapM_ onApp args
      onApp (Var _ _) = return ()
      onApp (Case _ es as) =
        mapM_ onApp es *> mapM_ (mapCaseAlternativeM_ onApp) as
      onApp (Let _ bs e) =
        mapM_ (mapBindM_ onApp) bs *> onApp e

-- |
-- DCE not used instance members.
--
-- instances that are not used can be turned into plain empty objects `{}`
-- memerbs that are not used can be truend into `function() {}`
--
-- One could provide a safe option where accessing a property of dce'ed
-- indnstance raises an informative error, the same for methods,
-- ```
-- function() {throw Error('this apple felt from the tree ;)');}
-- ```
transformExpr :: [(Qualified Ident, [Ident])] -> Expr Ann -> Expr Ann
transformExpr = undefined
