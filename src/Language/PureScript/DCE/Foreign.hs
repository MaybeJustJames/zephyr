{-# LANGUAGE BangPatterns #-}

-- |
-- Simple dead call elimination in foreign modules.
module Language.PureScript.DCE.Foreign
  ( runForeignModuleDeadCodeElimination,
  )
where

import Data.Foldable (foldr')
import Data.Graph (graphFromEdges, reachable, transposeG)
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.JavaScript.Parser.AST
  ( JSArrayElement (..),
    JSBlock (..),
    JSClassElement (..),
    JSClassHeritage (..),
    JSCommaList (..),
    JSCommaTrailingList (..),
    JSExportClause (..),
    JSExportDeclaration (..),
    JSExportSpecifier (..),
    JSExpression (..),
    JSIdent (..),
    JSMethodDefinition (..),
    JSModuleItem (..),
    JSObjectProperty (..),
    JSStatement (..),
    JSSwitchParts (..),
    JSTemplatePart (..),
    JSTryCatch (..),
    JSTryFinally (..),
    JSVarInitializer (..),
  )
import Language.PureScript.Names (Ident, runIdent)

-- | Filter export statements in a foreign module.  This is not 100% safe.  It
-- might remove declarations that are used somewhere in the foreign module (for
-- example by using @'eval'@).
runForeignModuleDeadCodeElimination :: [Ident] -> [JSModuleItem] -> [JSModuleItem]
runForeignModuleDeadCodeElimination is items = nub allReachable
  where
    allReachable :: [JSModuleItem]
    allReachable = do
      ePoint <- mapMaybe (vertexForKey . runIdent) is
      reachableNode <- reachable (transposeG graph) ePoint
      pure $ jsM $ fromVertex reachableNode

    -- Build a graph of exports statements.  Its initial set of edges point from
    -- an export statement to all other export statements that are using it.
    -- When checking if we need to include that vartex we just check if there is
    -- a path from a vertex to one of `entryPointVertices`.
    exps :: [JSModuleItem]
    exps = filter isExportModuleItem items

    (graph, fromVertex, vertexForKey) = graphFromEdges verts

    jsM :: (JSModuleItem, Text, [Text]) -> JSModuleItem
    jsM (a, _, _) = a

    verts :: [(JSModuleItem, Text, [Text])]
    verts = items >>= toVert
      where
        toVert :: JSModuleItem -> [(JSModuleItem, Text, [Text])]
        toVert item@(JSModuleExportDeclaration _ jsExportDec) = mkVertex item <$> exportNames jsExportDec
        toVert item@(JSModuleStatementListItem stmt) = mkVertex item <$> toplevelNames stmt
        toVert _ = []

        mkVertex :: JSModuleItem -> Text -> (JSModuleItem, Text, [Text])
        mkVertex item name =
          (item, name, foldr' (accumulateDependents name) [] exps)

        accumulateDependents :: Text -> JSModuleItem -> [Text] -> [Text]
        accumulateDependents name item acc =
          if isNameUsedByItem name item
            then exportedModuleItemName item ++ acc
            else acc

-- | foldr over `JSCommaList`
foldrJSCommaList :: (a -> b -> b) -> JSCommaList a -> b -> b
foldrJSCommaList _ JSLNil b = b
foldrJSCommaList fn (JSLOne a) !b = fn a b
foldrJSCommaList fn (JSLCons as _ a) !b = foldrJSCommaList fn as (fn a b)

toList :: JSCommaList a -> [a]
toList ls = foldrJSCommaList (:) ls []

toplevelNames :: JSStatement -> [Text]
toplevelNames (JSFunction _ (JSIdentName _ name) _ _ _ _ _) = [T.pack name]
toplevelNames (JSConstant _ exprs _) = catMaybes $ foldrJSCommaList (\name acc -> jsIdent name : acc) exprs []
toplevelNames _ = []

exportName' :: JSExportSpecifier -> Maybe Text
exportName' (JSExportSpecifier (JSIdentName _ name)) = Just $ T.pack name
exportName' _ = Nothing

jsIdent :: JSExpression -> Maybe Text
jsIdent (JSVarInitExpression ident _) = jsIdent ident
jsIdent (JSIdentifier _ i) = Just $ T.pack i
jsIdent (JSStringLiteral _ s) = Just $ T.pack s
jsIdent _ = Nothing

exportNames :: JSExportDeclaration -> [Text]
exportNames (JSExportLocals (JSExportClause _ names _) _) = catMaybes $ foldrJSCommaList (\ex exs -> exportName' ex : exs) names []
exportNames (JSExportFrom (JSExportClause _ names _) _ _) = catMaybes $ foldrJSCommaList (\ex exs -> exportName' ex : exs) names []
exportNames (JSExport (JSConstant _ names _) _) = catMaybes $ foldrJSCommaList (\name nms -> jsIdent name : nms) names []
exportNames (JSExport (JSFunction _ (JSIdentName _ name) _ _ _ _ _) _) = [T.pack name]
exportNames _ = []

isExportModuleItem :: JSModuleItem -> Bool
isExportModuleItem (JSModuleExportDeclaration _ _) = True
isExportModuleItem _ = False

exportedModuleItemName :: JSModuleItem -> [Text]
exportedModuleItemName (JSModuleExportDeclaration _ jsModuleExport) = exportNames jsModuleExport
exportedModuleItemName _ = []

isNameUsedByItem :: Text -> JSModuleItem -> Bool
isNameUsedByItem _ (JSModuleImportDeclaration _ _) = False
isNameUsedByItem _ (JSModuleExportDeclaration _ (JSExportFrom _ _ _)) = False
isNameUsedByItem name (JSModuleExportDeclaration _ decl@(JSExportLocals _ _)) = name `elem` exportNames decl
isNameUsedByItem name (JSModuleExportDeclaration _ (JSExport statement _)) = isNameUsedByStatement name statement
isNameUsedByItem name (JSModuleStatementListItem stmt) = isNameUsedByStatement name stmt

isNameUsedByStatement :: Text -> JSStatement -> Bool
isNameUsedByStatement name (JSStatementBlock _ block _ _) =
  any (isNameUsedByStatement name) block
isNameUsedByStatement _ (JSBreak _ _ _) = False
isNameUsedByStatement name (JSLet _ exprs _) =
  any (isNameUsedByExpr name) (toList exprs)
isNameUsedByStatement name (JSClass _ _ heritage _ cls _ _) =
  isNameUsedInHeritage name heritage || any (isNameUsedByClassElement name) cls
isNameUsedByStatement name (JSConstant _ exprs _) =
  any (isNameUsedByExpr name) (toList exprs)
isNameUsedByStatement _ (JSContinue _ _ _) = False
isNameUsedByStatement name (JSDoWhile _ stmt _ _ expr _ _) =
  isNameUsedByStatement name stmt || isNameUsedByExpr name expr
isNameUsedByStatement name (JSFor _ _ exprs1 _ exprs2 _ exprs3 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) (toList exprs1 ++ toList exprs2 ++ toList exprs3)
isNameUsedByStatement name (JSForIn _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSForVar _ _ _ exprs1 _ exprs2 _ exprs3 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) (toList exprs1 ++ toList exprs2 ++ toList exprs3)
isNameUsedByStatement name (JSForLet _ _ _ exprs1 _ exprs2 _ exprs3 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) (toList exprs1 ++ toList exprs2 ++ toList exprs3)
isNameUsedByStatement name (JSForConst _ _ _ exprs1 _ exprs2 _ exprs3 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) (toList exprs1 ++ toList exprs2 ++ toList exprs3)
isNameUsedByStatement name (JSForVarIn _ _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSForLetIn _ _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSForConstIn _ _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSForVarOf _ _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSForLetOf _ _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSForConstOf _ _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSForOf _ _ expr1 _ expr2 _ stmt) =
  isNameUsedByStatement name stmt || any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByStatement name (JSFunction _ _ _ exprs _ (JSBlock _ stmts _) _) =
  any (isNameUsedByExpr name) (toList exprs) || any (isNameUsedByStatement name) stmts
isNameUsedByStatement name (JSGenerator _ _ _ _ exprs _ (JSBlock _ stmts _) _) =
  any (isNameUsedByExpr name) (toList exprs) || any (isNameUsedByStatement name) stmts
isNameUsedByStatement name (JSIf _ _ expr _ stmt) = isNameUsedByExpr name expr || isNameUsedByStatement name stmt
isNameUsedByStatement name (JSIfElse _ _ expr _ stmtIf _ stmtElse) =
  isNameUsedByExpr name expr || any (isNameUsedByStatement name) [stmtIf, stmtElse]
isNameUsedByStatement name (JSLabelled _ _ stmt) = isNameUsedByStatement name stmt
isNameUsedByStatement _ (JSEmptyStatement _) = False
isNameUsedByStatement name (JSExpressionStatement expr _) = isNameUsedByExpr name expr
isNameUsedByStatement name (JSAssignStatement lhs _ rhs _) = any (isNameUsedByExpr name) [lhs, rhs]
isNameUsedByStatement name (JSMethodCall expr _ exprs _ _) = any (isNameUsedByExpr name) (expr : toList exprs)
isNameUsedByStatement _ (JSReturn _ Nothing _) = False
isNameUsedByStatement name (JSReturn _ (Just expr) _) = isNameUsedByExpr name expr
isNameUsedByStatement name (JSSwitch _ _ expr _ _ cases _ _) =
  isNameUsedByExpr name expr || any (isNameUsedByCase name) cases
isNameUsedByStatement name (JSThrow _ expr _) = isNameUsedByExpr name expr
isNameUsedByStatement name (JSTry _ (JSBlock _ stmts _) tryCatch tryFinally) =
  any (isNameUsedByStatement name) stmts || any (isNameUsedByTryCatch name) tryCatch || isNameUsedByTryFinally name tryFinally
isNameUsedByStatement name (JSVariable _ exprs _) = any (isNameUsedByExpr name) (toList exprs)
isNameUsedByStatement name (JSWhile _ _ expr _ stmt) = isNameUsedByExpr name expr || isNameUsedByStatement name stmt
isNameUsedByStatement name (JSWith _ _ expr _ stmt _) = isNameUsedByExpr name expr || isNameUsedByStatement name stmt

isNameUsedByExpr :: Text -> JSExpression -> Bool
isNameUsedByExpr name (JSIdentifier _ ident) = name == T.pack ident
isNameUsedByExpr _ (JSDecimal _ _) = False
isNameUsedByExpr _ (JSLiteral _ _) = False
isNameUsedByExpr _ (JSHexInteger _ _) = False
isNameUsedByExpr _ (JSOctal _ _) = False
isNameUsedByExpr _ (JSStringLiteral _ _) = False
isNameUsedByExpr _ (JSRegEx _ _) = False
isNameUsedByExpr name (JSArrayLiteral _ elems _) = any (isNameUsedByArrayElement name) elems
isNameUsedByExpr name (JSAssignExpression lhs _ rhs) = any (isNameUsedByExpr name) [lhs, rhs]
isNameUsedByExpr name (JSCallExpression expr _ exprs _) = any (isNameUsedByExpr name) (expr : toList exprs)
isNameUsedByExpr name (JSCallExpressionDot expr1 _ expr2) = any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByExpr name (JSCallExpressionSquare expr1 _ expr2 _) = any (isNameUsedByExpr name) [expr1, expr2]
isNameUsedByExpr name (JSClassExpression _ _ heritage _ cls _) =
  isNameUsedInHeritage name heritage || any (isNameUsedByClassElement name) cls
isNameUsedByExpr name (JSCommaExpression expr1 _ expr2) = isNameUsedByExpr name expr1 || isNameUsedByExpr name expr2
isNameUsedByExpr name (JSExpressionBinary expr1 _ expr2) = isNameUsedByExpr name expr1 || isNameUsedByExpr name expr2
isNameUsedByExpr name (JSExpressionParen _ expr _) = isNameUsedByExpr name expr
isNameUsedByExpr name (JSExpressionPostfix expr _) = isNameUsedByExpr name expr
isNameUsedByExpr name (JSExpressionTernary cond _ yes _ no) = any (isNameUsedByExpr name) [cond, yes, no]
isNameUsedByExpr name (JSArrowExpression _ _ stmt) = isNameUsedByStatement name stmt
isNameUsedByExpr name (JSFunctionExpression _ _ _ exprs _ (JSBlock _ stmts _)) =
  any (isNameUsedByExpr name) (toList exprs) || any (isNameUsedByStatement name) stmts
isNameUsedByExpr name (JSGeneratorExpression _ _ _ _ exprs _ (JSBlock _ stmts _)) =
  any (isNameUsedByExpr name) (toList exprs) || any (isNameUsedByStatement name) stmts
isNameUsedByExpr name (JSMemberDot expr1 _ expr2) = isNameUsedByExpr name expr1 || isNameUsedByExpr name expr2
isNameUsedByExpr name (JSMemberExpression expr _ exprs _) = any (isNameUsedByExpr name) (expr : toList exprs)
isNameUsedByExpr name (JSMemberNew _ expr _ exprs _) = any (isNameUsedByExpr name) (expr : toList exprs)
isNameUsedByExpr name (JSMemberSquare expr1 _ expr2 _) = isNameUsedByExpr name expr1 || isNameUsedByExpr name expr2
isNameUsedByExpr name (JSNewExpression _ expr) = isNameUsedByExpr name expr
isNameUsedByExpr name (JSObjectLiteral _ (JSCTLComma props _) _) = any (isNameUsedByObjectProperty name) (toList props)
isNameUsedByExpr name (JSObjectLiteral _ (JSCTLNone props) _) = any (isNameUsedByObjectProperty name) (toList props)
isNameUsedByExpr name (JSSpreadExpression _ expr) = isNameUsedByExpr name expr
isNameUsedByExpr name (JSTemplateLiteral Nothing _ _ parts) = any (isNameUsedByTemplatePart name) parts
isNameUsedByExpr name (JSTemplateLiteral (Just expr) _ _ parts) = isNameUsedByExpr name expr || any (isNameUsedByTemplatePart name) parts
isNameUsedByExpr name (JSUnaryExpression _ expr) = isNameUsedByExpr name expr
isNameUsedByExpr name (JSVarInitExpression expr initializer) = isNameUsedByExpr name expr || isNameUsedByVarInit name initializer
isNameUsedByExpr _ (JSYieldExpression _ Nothing) = False
isNameUsedByExpr name (JSYieldExpression _ (Just expr)) = isNameUsedByExpr name expr
isNameUsedByExpr name (JSYieldFromExpression _ _ expr) = isNameUsedByExpr name expr

isNameUsedInHeritage :: Text -> JSClassHeritage -> Bool
isNameUsedInHeritage _ JSExtendsNone = False
isNameUsedInHeritage name (JSExtends _ expr) = isNameUsedByExpr name expr

isNameUsedByClassElement :: Text -> JSClassElement -> Bool
isNameUsedByClassElement _ (JSClassSemi _) = False
isNameUsedByClassElement name (JSClassInstanceMethod defn) = isNameUsedByMethodDefinition name defn
isNameUsedByClassElement name (JSClassStaticMethod _ defn) = isNameUsedByMethodDefinition name defn

isNameUsedByMethodDefinition :: Text -> JSMethodDefinition -> Bool
isNameUsedByMethodDefinition name (JSMethodDefinition _ _ exprs _ _) = any (isNameUsedByExpr name) (toList exprs)
isNameUsedByMethodDefinition name (JSGeneratorMethodDefinition _ _ _ exprs _ _) = any (isNameUsedByExpr name) (toList exprs)
isNameUsedByMethodDefinition name (JSPropertyAccessor _ _ _ exprs _ _) = any (isNameUsedByExpr name) (toList exprs)

isNameUsedByCase :: Text -> JSSwitchParts -> Bool
isNameUsedByCase name (JSDefault _ _ stmts) = any (isNameUsedByStatement name) stmts
isNameUsedByCase name (JSCase _ expr _ stmts) = isNameUsedByExpr name expr || any (isNameUsedByStatement name) stmts

isNameUsedByTryCatch :: Text -> JSTryCatch -> Bool
isNameUsedByTryCatch name (JSCatch _ _ expr _ (JSBlock _ stmts _)) = isNameUsedByExpr name expr || any (isNameUsedByStatement name) stmts
isNameUsedByTryCatch name (JSCatchIf _ _ expr1 _ expr2 _ (JSBlock _ stmts _)) = any (isNameUsedByExpr name) [expr1, expr2] || any (isNameUsedByStatement name) stmts

isNameUsedByTryFinally :: Text -> JSTryFinally -> Bool
isNameUsedByTryFinally name (JSFinally _ (JSBlock _ stmts _)) = any (isNameUsedByStatement name) stmts
isNameUsedByTryFinally _ JSNoFinally = False

isNameUsedByArrayElement :: Text -> JSArrayElement -> Bool
isNameUsedByArrayElement _ (JSArrayComma _) = False
isNameUsedByArrayElement name (JSArrayElement expr) = isNameUsedByExpr name expr

isNameUsedByObjectProperty :: Text -> JSObjectProperty -> Bool
isNameUsedByObjectProperty name (JSPropertyNameandValue _ _ exprs) = any (isNameUsedByExpr name) exprs
isNameUsedByObjectProperty name (JSPropertyIdentRef _ ref) = name == T.pack ref
isNameUsedByObjectProperty name (JSObjectMethod mDef) = isNameUsedByMethodDefinition name mDef

isNameUsedByTemplatePart :: Text -> JSTemplatePart -> Bool
isNameUsedByTemplatePart name (JSTemplatePart expr _ _) = isNameUsedByExpr name expr

isNameUsedByVarInit :: Text -> JSVarInitializer -> Bool
isNameUsedByVarInit name (JSVarInit _ expr) = isNameUsedByExpr name expr
isNameUsedByVarInit _ JSVarInitNone = False
