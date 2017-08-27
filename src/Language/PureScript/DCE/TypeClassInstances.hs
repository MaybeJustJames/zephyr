-- Tree shaking of type classes instance methods on CoreFn
module Language.PureScript.DCE.TypeClassInstances
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
import           Language.PureScript.CoreFn
import           Language.PureScript.Names

data InstanceDict = InstanceDict
  { instTypeClass :: Qualified (ProperName 'ClassName)
  , instExpr :: Expr Ann
  }

-- |
-- Dictionary of all instances accross all modules.
--
-- It allows to efficiently check if an identifier used in an expression is an
-- instance declaration.
type Instances = M.Map (Qualified Ident) InstanceDict 

isInstance :: Qualified Ident -> Instances -> Bool
isInstance (Qualified Nothing _) _ = False
isInstance ident dict = ident `M.member` dict

dceInstances :: forall t. [ModuleT t Ann] -> [ModuleT t Ann]
dceInstances mods = undefined
  where
  instanceDict :: Instances
  instanceDict = M.fromList (instancesInModule `concatMap` mods)
    where
    instancesInModule :: ModuleT t Ann -> [(Qualified Ident, InstanceDict)]
    instancesInModule =
        concatMap instanceDicts
      . uncurry zip
      . first repeat
      . (moduleName &&& moduleDecls)

    instanceDicts :: (ModuleName, Bind Ann) -> [(Qualified Ident, InstanceDict)]
    instanceDicts (mn, NonRec _ i e)  | Just tyClsName <- isInstanceOf e = [(mkQualified i mn, InstanceDict tyClsName e)]
                                      | otherwise = []
    instanceDicts (mn, Rec bs) = mapMaybe
      (\((_, i), e) ->
        case isInstanceOf e of
          Just tyClsName  -> Just (mkQualified i mn, InstanceDict tyClsName e)
          Nothing         -> Nothing)
      bs
  
-- | 
-- Check if an expression is constrained.
--
-- This requires types to be recoverable from [CoreFn
-- representation](https://github.com/coot/purescript/commit/29ef583ae754ca963dc245e8a1b63445f1eeb90e).
isConstrained :: Expr Ann -> Bool
isConstrained (Abs (_, _, Just (P.ConstrainedType _ _), _) _ _) = True
isConstrained _ = False

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
exprInstances :: Instances -> Expr Ann -> [Qualified Ident]
exprInstances d = go
  where
  (_, go, _, _) = everythingOnValues (++) (const []) onExpr (const []) (const [])

  onExpr :: Expr Ann -> [Qualified Ident]
  onExpr (Var _ i) | i `isInstance` d = [i]
  onExpr _ = []

data TypeClassInstDepsData = TypeClassInstDeps
  { tciClassName :: Qualified (ProperName 'ClassName)
  , tciName :: Text
  }
type TypeClassInstDeps = Cofree Maybe TypeClassInstDepsData
-- ^
-- Tree structure that encodes information about type
-- class instance dependencies for constrained types.  Each constrain will map
-- to one `TypeClassInstanceDeps`.
--
-- `tciClassName` is the _TypeClass_ name
-- `tciName` is the field name of a member or parent type class used in
-- generated code.  This information is available in
-- `Language.PureScript.CoreFn.Meta` (see
-- [corefn-typeclasses](https://github.com/coot/purescript/blob/corefn-typeclasses/src/Language/PureScript/CoreFn/Meta.hs#L29)
-- branch of my `purescript` repo clone).

-- |
-- Find all instance dependencies of an expression with a constrained type
exprInstDeps :: Expr Ann -> [TypeClassInstDeps]
exprInstDeps = undefined

-- |
-- For a given _constrained_ expression, we need to find out all the instances
-- that are used.  For each set of them we pair them with the corresponsing
-- `TypeClassInstDeps` and compute all memeber that are used.
compDeps :: [(InstanceDict, TypeClassInstDeps)] -> [(Qualified Ident, [Ident])]
compDeps = undefined

-- |
-- Find all instance dependencies of a class member instance.
-- The result is an instance name with the list of all members of its class
-- that are used.
-- 
-- This is much simpler that `exprInstDeps` since in member declarations,
-- instances are mentioned directly.
memberDeps :: Expr Ann -> [(Qualified Ident, [Ident])]
memberDeps = undefined

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
