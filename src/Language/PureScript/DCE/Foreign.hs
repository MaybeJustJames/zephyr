{-# LANGUAGE BangPatterns #-}

-- |
-- Simple dead call elimination in foreign modules.
module Language.PureScript.DCE.Foreign
  ( runForeignModuleDeadCodeElimination
  ) where

import           Prelude.Compat
import Control.Monad ( guard )
import Data.Graph ( graphFromEdges, path, Vertex )
import           Data.Foldable (foldr')
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.JavaScript.Parser.AST
                  ( JSStatement(..)
                  , JSExpression(..)
                  , JSCommaList(..)
                  , JSBlock(..)
                  , JSSwitchParts(..)
                  , JSTryCatch(..)
                  , JSTryFinally(..)
                  , JSArrayElement(..)
                  , JSObjectProperty(..)
                  , JSCommaTrailingList(..)
                  , JSCommaList(..)
                  , JSMethodDefinition(..)
                  , JSClassElement(..)
                  , JSClassHeritage(..)
                  , JSTemplatePart(..), JSModuleItem (JSModuleStatementListItem)
                  )
import Language.PureScript.Names ( runIdent, Ident )

-- | foldr over `JSCommaList`
foldrJSCommaList :: (a -> b -> b) -> JSCommaList a -> b -> b
foldrJSCommaList _ JSLNil b = b
foldrJSCommaList fn (JSLOne a) !b = fn a b
foldrJSCommaList fn (JSLCons as _ a) !b = foldrJSCommaList fn as (fn a b)

-- | Filter export statements in a foreign module.  This is not 100% safe.  It
-- might remove declarations that are used somewhere in the foreign module (for
-- example by using @'eval'@).
--
runForeignModuleDeadCodeElimination :: [Ident] -> [JSModuleItem] -> [JSModuleItem]
runForeignModuleDeadCodeElimination is items = filter filterExports items
  where
    filterExports :: JSModuleItem -> Bool
    filterExports (JSModuleStatementListItem (JSAssignStatement (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ x) _) _ _ _))
      = fltr (unquote . T.pack $ x)
    filterExports (JSModuleStatementListItem (JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ x)) _ _ _))
      = fltr (T.pack x)
    filterExports _ = True

    fltr :: Text -> Bool
    fltr t = any (fromMaybe True . (path graph <$> vertexForKey t <*>) . Just) entryPointVertices
          -- one of `entryPointVertices` depend on this vertex
          || any (isUsedInItem t) nonExps
          -- it is used in any non export statements

    -- Build a graph of exports statements.  Its initial set of edges point from
    -- an export statement to all other export statements that are using it.
    -- When checking if we need to include that vartex we just check if there is
    -- a path from a vertex to one of `entryPointVertices`.
    exps :: [JSModuleItem]
    exps = filter isExportStatement items

    nonExps = filter (not . isExportStatement) items

    (graph, _, vertexForKey) = graphFromEdges verts

    verts :: [(JSModuleItem, Text, [Text])]
    verts = mapMaybe toVert exps
      where
      toVert :: JSModuleItem -> Maybe (JSModuleItem, Text, [Text])
      toVert item
        | Just name <- exportStatementName item = Just (item, name, foldr' (fn name) [] exps)
        | otherwise = Nothing

      fn name item' nms
        | isUsedInItem name item'
        , Just n <- exportStatementName item' = n:nms
        | otherwise = nms

    entryPointVertices :: [Vertex]
    entryPointVertices = catMaybes $ do
      (_, k, _) <- verts
      guard $ k `elem` ns
      return (vertexForKey k)
      where
      ns = runIdent <$> is

    unquote :: Text -> Text
    unquote = T.drop 1 . T.dropEnd 1

    isExportStatement :: JSModuleItem -> Bool
    isExportStatement (JSModuleStatementListItem (JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ _)) _ _ _)) = True
    isExportStatement (JSModuleStatementListItem (JSAssignStatement (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ _) _) _ _ _)) = True
    isExportStatement _ = False

    exportStatementName :: JSModuleItem -> Maybe Text
    exportStatementName (JSModuleStatementListItem (JSAssignStatement (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ i)) _ _ _)) = Just . T.pack $ i
    exportStatementName (JSModuleStatementListItem (JSAssignStatement (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ i) _) _ _ _)) = Just . unquote . T.pack $ i
    exportStatementName _ = Nothing

    isUsedInItem :: Text -> JSModuleItem -> Bool
    isUsedInItem n (JSModuleStatementListItem stmt) = isUsedInStmt n stmt
    isUsedInItem _ _ = False

    -- Check if (export) identifier is used within a JSStatement.
    isUsedInStmt :: Text -> JSStatement -> Bool
    isUsedInStmt n (JSStatementBlock _ ss _ _) = any (isUsedInStmt n) ss
    isUsedInStmt n (JSLet _ es _) = isUsedInExprs n es
    isUsedInStmt n (JSClass _ _ h _ cs _ _) =
      isUsedInClassHeritage n h || any (isUsedInClassElement n) cs
    isUsedInStmt n (JSDoWhile _ stm _ _ e _ _) = isUsedInStmt n stm || isUsedInExpr n e
    isUsedInStmt n (JSFor _ _ es1 _ es2 _ es3 _ s) = isUsedInExprs n es1 || isUsedInExprs n es2 || isUsedInExprs n es3 || isUsedInStmt n s
    isUsedInStmt n (JSForIn _ _ e1 _ e2 _ s) = isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    isUsedInStmt n (JSForVar _ _ _ es1 _ es2 _ es3 _ s) = isUsedInExprs n es1 || isUsedInExprs n es2 || isUsedInExprs n es3 || isUsedInStmt n s
    isUsedInStmt n (JSForVarIn _ _ _ e1 _ e2 _ s) = isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    isUsedInStmt n (JSForLet _ _ _ es1 _ es2 _ es3 _ s) =
      isUsedInExprs n es1 || isUsedInExprs n es2 || isUsedInExprs n es3 ||
      isUsedInStmt n s
    isUsedInStmt n (JSForLetIn _ _ _ e1 _ e2 _ s) =
      isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    isUsedInStmt n (JSForLetOf _ _ _ e1 _ e2 _ s) =
      isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    isUsedInStmt n (JSForConst _ _ _ es1 _ es2 _ es3 _ s) =
      isUsedInExprs n es1 || isUsedInExprs n es2 || isUsedInExprs n es3 || isUsedInStmt n s
    isUsedInStmt n (JSForConstIn _ _ _ e1 _ e2 _ s) =
      isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    isUsedInStmt n (JSForConstOf _ _ _ e1 _ e2 _ s) =
      isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    isUsedInStmt n (JSForOf _ _ e1 _ e2 _ s) =
      isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    isUsedInStmt n (JSForVarOf _ _ _ e1 _ e2 _ s) =
      isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInStmt n s
    {--
      - isUsedInStmt n (JSAsyncFunction _ _ _ _ es _ (JSBlock _ ss _) _) =
      -   isUsedInExprs n es || any (isUsedInStmt n) ss
      --}
    isUsedInStmt n (JSFunction _ _ _ es _ (JSBlock _ ss _) _) =
      isUsedInExprs n es || any (isUsedInStmt n) ss
    isUsedInStmt n (JSGenerator _ _ _ _ es _ (JSBlock _ ss _) _) =
      isUsedInExprs n es || any (isUsedInStmt n) ss
    isUsedInStmt n (JSIf _ _ e _ s) = isUsedInExpr n e || isUsedInStmt n s
    isUsedInStmt n (JSIfElse _ _ e _ s1 _ s2) = isUsedInExpr n e || isUsedInStmt n s1 || isUsedInStmt n s2
    isUsedInStmt n (JSLabelled _ _ s) = isUsedInStmt n s
    isUsedInStmt _ (JSEmptyStatement _) = False
    isUsedInStmt n (JSExpressionStatement e _) = isUsedInExpr n e
    isUsedInStmt n (JSAssignStatement e1 _ e2 _) = isUsedInExpr n e1 || isUsedInExpr n e2
    isUsedInStmt n (JSMethodCall e _ es _ _) = isUsedInExpr n e || isUsedInExprs n es
    isUsedInStmt n (JSReturn _ me _) = maybe False (isUsedInExpr n) me
    isUsedInStmt n (JSSwitch _ _ e _ _ sps _ _) = isUsedInExpr n e || any (isUsedInSwitchParts n) sps
    isUsedInStmt n (JSThrow _ e _) = isUsedInExpr n e
    isUsedInStmt n (JSTry _ (JSBlock _ ss _) cs f) = any (isUsedInStmt n) ss || any (isUsedInTryCatch n) cs || isUsedInFinally n f
    isUsedInStmt n (JSVariable _ es _) = isUsedInExprs n es
    isUsedInStmt n (JSWhile _ _ e _ s) = isUsedInExpr n e || isUsedInStmt n s
    isUsedInStmt n (JSWith _ _ e _ s _) = isUsedInExpr n e || isUsedInStmt n s
    isUsedInStmt _ JSBreak{} = False
    isUsedInStmt _ JSConstant{} = False
    isUsedInStmt _ JSContinue{} = False

    -- Check if an exported identifier is used within a 'JSExpression'
    isUsedInExpr :: Text -> JSExpression -> Bool
    isUsedInExpr n (JSMemberDot (JSIdentifier _ "exports") _ (JSIdentifier _ i)) = n == T.pack i
    isUsedInExpr n (JSMemberDot e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
    isUsedInExpr n (JSArrayLiteral _ as _) = any (isUsedInArrayElement n) as
    isUsedInExpr n (JSAssignExpression e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
    -- isUsedInExpr n (JSAwaitExpression _ e) = isUsedInExpr n e
    isUsedInExpr n (JSCallExpression e _ es _) = isUsedInExpr n e || isUsedInExprs n es
    isUsedInExpr n (JSCallExpressionDot e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
    isUsedInExpr n (JSCallExpressionSquare e1 _ e2 _) = isUsedInExpr n e1 || isUsedInExpr n e2
    isUsedInExpr n (JSClassExpression _ _ h _ cs _) =
      isUsedInClassHeritage n h || any (isUsedInClassElement n) cs
    isUsedInExpr n (JSExpressionBinary e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
    isUsedInExpr n (JSExpressionParen _ e _) = isUsedInExpr n e
    isUsedInExpr n (JSExpressionPostfix e _) = isUsedInExpr n e
    isUsedInExpr n (JSExpressionTernary e1 _ e2 _ e3) = isUsedInExpr n e1 || isUsedInExpr n e2 || isUsedInExpr n e3
    isUsedInExpr n (JSArrowExpression _ _ s) = isUsedInStmt n s
    isUsedInExpr n (JSFunctionExpression _ _ _ _ _ (JSBlock _ ss _)) = any (isUsedInStmt n) ss
    isUsedInExpr n (JSGeneratorExpression _ _ _ _ es _ (JSBlock _ ss _)) =
      isUsedInExprs n es || any (isUsedInStmt n) ss
    isUsedInExpr n (JSMemberExpression e _ es _) = isUsedInExpr n e || isUsedInExprs n es
    isUsedInExpr n (JSMemberNew _ e _ es _) = isUsedInExpr n e || isUsedInExprs n es
    isUsedInExpr n (JSMemberSquare (JSIdentifier _ "exports") _ (JSStringLiteral _ i) _) = n == (unquote .T.pack $ i)
    isUsedInExpr n (JSMemberSquare e1 _ e2 _) = isUsedInExpr n e1 || isUsedInExpr n e2
    isUsedInExpr n (JSNewExpression _ e) = isUsedInExpr n e
    isUsedInExpr n (JSObjectLiteral _ ops _) = foldrJSCommaList (\p b -> isUsedInObjectProperty n p || b) (fromCTList ops) False
      where
      fromCTList (JSCTLComma as _) = as
      fromCTList (JSCTLNone as) = as
    isUsedInExpr n (JSSpreadExpression _ e) = isUsedInExpr n e
    isUsedInExpr n (JSTemplateLiteral me _ _ tps) =
      any (isUsedInExpr n) me || any (\(JSTemplatePart e _ _) -> isUsedInExpr n e) tps
    isUsedInExpr n (JSUnaryExpression _ e) = isUsedInExpr n e
    isUsedInExpr n (JSVarInitExpression e _) = isUsedInExpr n e
    isUsedInExpr _ JSIdentifier{} = False
    isUsedInExpr _ JSDecimal{} = False
    isUsedInExpr _ JSLiteral{} = False
    isUsedInExpr _ JSHexInteger{} = False
    isUsedInExpr _ JSOctal{} = False
    isUsedInExpr _ JSStringLiteral{} = False
    isUsedInExpr _ JSRegEx{} = False
    isUsedInExpr n (JSCommaExpression e1 _ e2) = isUsedInExpr n e1 || isUsedInExpr n e2
    isUsedInExpr n (JSYieldExpression _ me) = any (isUsedInExpr n) me
    isUsedInExpr n (JSYieldFromExpression _ _ e) = isUsedInExpr n e

    isUsedInExprs :: Text -> JSCommaList JSExpression -> Bool
    isUsedInExprs n es = foldrJSCommaList fn es False
      where
      fn :: JSExpression -> Bool -> Bool
      fn e b = isUsedInExpr n e || b

    -- Check if (export) identifier is used withing a JSSitchParts
    isUsedInSwitchParts :: Text -> JSSwitchParts -> Bool
    isUsedInSwitchParts n (JSCase _ e _ ss) = isUsedInExpr n e || any (isUsedInStmt n) ss
    isUsedInSwitchParts n (JSDefault _ _ ss) = any (isUsedInStmt n) ss

    -- Check if (export) identifier is used withing a JSTryCatch
    isUsedInTryCatch :: Text -> JSTryCatch -> Bool
    isUsedInTryCatch n (JSCatch _ _ e _ (JSBlock _ ss _)) = isUsedInExpr n e || any (isUsedInStmt n) ss
    isUsedInTryCatch n (JSCatchIf _ _ e1 _ e2 _ (JSBlock _ ss _)) = isUsedInExpr n e1 || isUsedInExpr n e2 || any (isUsedInStmt n) ss

    -- |
    -- Check if (export) identifier is used withing a JSTryFinally
    isUsedInFinally :: Text -> JSTryFinally -> Bool
    isUsedInFinally n (JSFinally _ (JSBlock _ ss _)) = any (isUsedInStmt n) ss
    isUsedInFinally _ JSNoFinally = False

    -- |
    -- Check if (export) identifier is used withing a JSArrayElement
    isUsedInArrayElement :: Text -> JSArrayElement -> Bool
    isUsedInArrayElement n (JSArrayElement e) = isUsedInExpr n e
    isUsedInArrayElement _ JSArrayComma{} = False

    -- |
    -- Check if (export) identifier is used withing a JSObjectProperty
    isUsedInObjectProperty :: Text -> JSObjectProperty -> Bool
    isUsedInObjectProperty n (JSPropertyNameandValue _ _ es) = any (isUsedInExpr n) es
    isUsedInObjectProperty _ JSPropertyIdentRef{} = False
    isUsedInObjectProperty n (JSObjectMethod m) = isUsedInMethodDefinition n m

    isUsedInMethodDefinition :: Text -> JSMethodDefinition -> Bool
    isUsedInMethodDefinition n (JSMethodDefinition _ _ es _ (JSBlock _ ss _))
      = isUsedInExprs n es || any (isUsedInStmt n) ss
    isUsedInMethodDefinition n (JSGeneratorMethodDefinition _ _ _ es _ (JSBlock _ ss _))
      = isUsedInExprs n es || any (isUsedInStmt n) ss
    isUsedInMethodDefinition n (JSPropertyAccessor _ _ _ es _ (JSBlock _ ss _))
      = isUsedInExprs n es || any (isUsedInStmt n) ss

    isUsedInClassElement :: Text -> JSClassElement -> Bool
    isUsedInClassElement n (JSClassInstanceMethod m) = isUsedInMethodDefinition n m
    isUsedInClassElement n (JSClassStaticMethod _ m) = isUsedInMethodDefinition n m
    isUsedInClassElement _ JSClassSemi{}             = False

    isUsedInClassHeritage :: Text -> JSClassHeritage -> Bool
    isUsedInClassHeritage n (JSExtends _ e) = isUsedInExpr n e
    isUsedInClassHeritage _ JSExtendsNone   = False
