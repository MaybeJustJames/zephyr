-- Dead Code Elimination for CoreFn
module Language.PureScript.DCE.CoreFn
  ( dce
  , dceExpr
  ) where

import           Prelude.Compat
import           Control.Arrow ((***))
import           Control.Monad
import           Data.Graph
import           Data.Foldable (foldl', foldr')
import           Data.List (any, elem, filter, groupBy, sortBy)
import           Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import           Language.PureScript.CoreFn
import           Language.PureScript.Names

type Key = Qualified Ident

data DCEVertex a
  = BindVertex (Bind a)
  | ForeignVertex (Qualified Ident)

dce :: forall t a. Show a => [ModuleT t a] -> [Qualified Ident] -> [ModuleT t a]
dce modules [] = modules
dce modules entryPoints = do
    vs <- reachableList
    Module {..} <- modules
    guard (getModuleName vs == Just moduleName)
    let
        -- | filter declarations preserving the order
        decls :: [Bind a]
        decls = filter filterByIdents moduleDecls
          where
          declIdents :: [Ident]
          declIdents = concatMap toIdents vs

          toIdents :: (DCEVertex a, Key, [Key]) -> [Ident]
          toIdents (BindVertex b, _, _) = bindIdents b
          toIdents _                    = []

          filterByIdents :: Bind a -> Bool
          filterByIdents = any (`elem` declIdents) . bindIdents

        idents :: [Ident]
        idents = concatMap getBindIdents decls

        exports :: [Ident]
        exports = filter (`elem` (idents ++ fst `map` foreigns)) moduleExports

        mods :: [ModuleName]
        mods = mapMaybe getQual (concatMap (\(_, _, ks) -> ks) vs)

        imports :: [(a, ModuleName)]
        imports = filter ((`elem` mods) . snd) moduleImports

        foreigns :: [ForeignDeclT t]
        foreigns = filter ((`S.member` reachableSet) . Qualified (Just moduleName) . fst) moduleForeign
          where
            reachableSet = foldr' (\(_, k, ks) s -> S.insert k s `S.union` S.fromList ks) S.empty vs

    return $ Module moduleComments moduleName modulePath imports exports foreigns (dceExpr `map` decls)
  where
  (graph, keyForVertex, vertexForKey) = graphFromEdges verts

  bindIdents :: Bind a -> [Ident]
  bindIdents (NonRec _ i _) = [i]
  bindIdents (Rec l) = map (\((_, i), _) -> i) l

  -- | The Vertex set
  verts :: [(DCEVertex a, Key, [Key])]
  verts = do
      Module _ mn _ _ _ mf ds <- modules
      concatMap (toVertices mn) ds ++ ((\q -> (ForeignVertex q, q, [])) . flip mkQualified mn . fst) `map` mf
    where
    toVertices :: ModuleName -> Bind a -> [(DCEVertex a, Key, [Key])]
    toVertices mn b@(NonRec _ i e) = [(BindVertex b, mkQualified i mn, deps e)]
    toVertices mn b@(Rec bs) =
      let ks :: [(Key, [Key])]
          ks = map (\((_, i), e) -> (mkQualified i mn, deps e)) bs
      in map (\(k, ks') -> (BindVertex b, k, map fst ks ++ ks')) ks

    -- | Find dependencies of an expression
    deps :: Expr a -> [Key]
    deps = go
      where
        (_, go, _, _) = everythingOnValues (++)
          (const [])
          onExpr
          onBinder
          (const [])

        -- | Build graph only from qualified identifiers
        onExpr :: Expr a -> [Key]
        onExpr (Var _ i) = [i | isQualified i]
        onExpr _ = []

        onBinder :: Binder a -> [Key]
        onBinder (ConstructorBinder _ _ c _) = [fmap (Ident . runProperName) c]
        onBinder _ = []

  -- | Vertices corresponding to the entry points which we want to keep.
  entryPointVertices :: [Vertex]
  entryPointVertices = catMaybes $ do
    (_, k, _) <- verts
    guard $ k `elem` entryPoints
    return (vertexForKey k)

  -- | The list of reachable vertices grouped by module name
  reachableList :: [[(DCEVertex a, Key, [Key])]]
  reachableList
    = groupBy (\(_, k1, _) (_, k2, _) -> getQual k1 == getQual k2)
    $ sortBy (\(_, k1, _) (_, k2, _) -> getQual k1 `compare` getQual k2)
    $ map keyForVertex (concatMap (reachable graph) entryPointVertices)

  getModuleName :: [(DCEVertex a, Key, [Key])] -> Maybe ModuleName
  getModuleName [] = Nothing
  getModuleName ((_, k, _) : _) = getQual k

getBindIdents :: Bind a -> [Ident]
getBindIdents (NonRec _ i _) = [i]
getBindIdents (Rec is) = map (\((_, i), _) -> i) is

getBindIdentsWithExpr :: Bind a -> [(Ident, Expr a)]
getBindIdentsWithExpr (NonRec _ i e) = [(i,e)]
getBindIdentsWithExpr (Rec is) = map (\((_, i),e) -> (i, e)) is

-- DCE of local identifiers
-- detect and remove unused bindings
dceExpr :: forall a. Show a => Bind a -> Bind a
dceExpr = go
  where
  (go, _, _) = everywhereOnValues id exprFn id

  exprFn :: Expr a -> Expr a
  exprFn (Let ann bs ex) =
    let nbs = foldr' bindFn [] bs
    in if null nbs
      then ex
      else Let ann nbs ex

    where
    bindFn :: Bind a -> [Bind a] -> [Bind a]
    bindFn b@(NonRec _ i _) r | i `elem` reachableIdents = b : r
                              | otherwise = r
    bindFn (Rec l) r =
      let l' = filter (\((_, i), _) -> i `elem` reachableIdents) l
      in if null l'
          then r
          else Rec l' : r

    (graph, keyForVertex, vertexForKey) = graphFromEdges verts

    -- | Build list of vertices
    --
    -- Under the assumption that all identifiers are unique, which is
    -- fullfiled by PureScript.
    verts :: [(Ident, Ident, [Ident])]
    verts = do
      let bes = getBindIdentsWithExpr `concatMap` bs
      (i, e) <- bes
      let deps = fst `map` filter ((\i' -> i' /= i && isUsedInExpr i' e)  . fst) bes
      return (i, i, deps)

    entryPointVertices :: [Vertex]
    entryPointVertices = catMaybes $ do
      (_, i, _) <- verts
      guard $ isUsedInExpr i ex
      return (vertexForKey i)

    reachableIdents = map ((\(_, i, _) -> i) . keyForVertex) $ reachable graph `concatMap` entryPointVertices
  exprFn e = e

  isUsedInExpr :: Ident -> Expr a -> Bool
  isUsedInExpr i (Literal _ (ArrayLiteral es)) = any (isUsedInExpr i) es
  isUsedInExpr i (Literal _ (ObjectLiteral es)) = any (isUsedInExpr i . snd) es
  isUsedInExpr _ (Literal _ _) = False
  isUsedInExpr i (Constructor _ _ _ is) = i `elem` is
  isUsedInExpr i (Accessor _ _ e) = isUsedInExpr i e
  isUsedInExpr i (ObjectUpdate _ e ups) = isUsedInExpr i e || any (isUsedInExpr i . snd) ups
  isUsedInExpr i (App _ (Abs _ i' e) r)
    = if isUsedInExpr i r
        then isUsedInExpr i' e
        else isUsedInExpr i e
  isUsedInExpr i (App _ l r) = isUsedInExpr i l || isUsedInExpr i r
  isUsedInExpr i (Abs _ i' e) = i /= i' && isUsedInExpr i e
  isUsedInExpr i (Var _ qi) = qi == Qualified Nothing i
  isUsedInExpr i (Case _ es alts) = any (isUsedInExpr i) es || any (isUsedInCaseAlternative i) alts
  isUsedInExpr i (Let _ bs e) =
    if shadowed
        then used
        else used || isUsedInExpr i e
    where
    -- |
    -- Check if an identifier is used in bindings and the resulting
    -- expression.  A binding might shadow an identifier.  The first Boolean
    -- value denotes if i is used in any bind expression, the second if it was
    -- shadowed.
    (used, shadowed) = foldl' fn (False, False) (concatMap getBindIdentsWithExpr bs)

    fn (u, s) (i', e')
      | s || i == i'  = (u, True)
      | otherwise     = (u || isUsedInExpr i e', False)

  isUsedInCaseAlternative i (CaseAlternative bs ee) =
    not (any (\b -> case b of
                    VarBinder _ i'  -> i == i'
                    _               -> False) bs
        )
    &&
      (case ee of
        Right e -> isUsedInExpr i e
        Left es -> any (uncurry (||) . (isUsedInExpr i *** isUsedInExpr i)) es)
