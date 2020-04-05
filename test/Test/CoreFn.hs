module Test.CoreFn ( main )  where

import Prelude ()
import Prelude.Compat

import Data.List (concatMap, foldl', intersect)
import qualified Data.List as L

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn
import Language.PureScript.DCE
import Language.PureScript.Names
import Language.PureScript.PSString

import Test.Hspec
import Test.QuickCheck

import Test.Generators hiding (ann)

main :: IO ()
main = hspec spec

getNames :: Bind a -> [Ident]
getNames (NonRec _ i _) = [i]
getNames (Rec l) = (\((_, i), _) -> i) `map` l

hasIdent :: Ident -> [Bind Ann] -> Bool
hasIdent i = (i `elem`) . concatMap getNames

ann :: Ann
ann = ssAnn (SourceSpan "src/Test.purs" (SourcePos 0 0) (SourcePos 0 0))

prop_exprDepth :: PSExpr Ann -> Property
prop_exprDepth (PSExpr e) =
  let b = NonRec ann (Ident "x") e
      NonRec _ _ e' = runBindDeadCodeElimination b
      d  = exprDepth e
      d' = exprDepth e'
  in collect (10 * (d' * 100 `div` (10 * d)))
    $ counterexample (show e)
    $ d' <= d

prop_lets :: PSExpr Ann -> Property
prop_lets (PSExpr f) =
  let b = NonRec ann (Ident "x") f
      NonRec _ _ f' = runBindDeadCodeElimination b
      d  = countLets f
      d' = countLets f'
      idents = findBindIdents f'
  in label ((if d > 0 then show (10 * ((d' * 100 `div` d) `div` 10)) ++ "%" else "-") ++ " of removed let bindings")
    $ counterexample (show f)
    $  d' <= d
    && L.null (intersect idents unusedIdents)
  where
  countLets :: Expr a -> Int
  countLets (Literal _ (ArrayLiteral es)) = foldl' (\x e -> x + countLets e) 0 es
  countLets (Literal _ (ObjectLiteral o)) = foldl' (\x (_, e) -> x + countLets e) 0 o
  countLets Literal{} = 0
  countLets Constructor{} = 0
  countLets (Accessor _ _ e) = countLets e
  countLets (ObjectUpdate _ e o) = countLets e + foldl' (\x (_, e') -> x + countLets e') 0 o
  countLets (Abs _ _ e) = countLets e
  countLets (App _ e f') = countLets e + countLets f'
  countLets Var{} = 0
  countLets (Case _ es cs) = foldl' (\x e -> x + countLets e) 0 es + foldl countLetsInCaseAlternative 0 cs
    where
    countLetsInCaseAlternative x (CaseAlternative _ r) = 
      x + either (foldl' (\y (g, e) -> y + countLets g + countLets e) 0) countLets r
  countLets (Let _ _ e) = 1 + countLets e

  findBindIdents :: Expr a -> [Ident]
  findBindIdents (Literal _ (ArrayLiteral es)) = concatMap findBindIdents es
  findBindIdents (Literal _ (ObjectLiteral o)) = concatMap (findBindIdents . snd) o
  findBindIdents Literal{} = []
  findBindIdents Constructor{} = []
  findBindIdents (Accessor _ _ e) = findBindIdents e
  findBindIdents (ObjectUpdate _ e o) = findBindIdents e ++ concatMap (findBindIdents . snd) o
  findBindIdents (Abs _ _ e) = findBindIdents e
  findBindIdents (App _ e f') = findBindIdents e ++ findBindIdents f'
  findBindIdents Var{} = []
  findBindIdents (Case _ es cs) = concatMap findBindIdents es ++ concatMap countLetsInCaseAlternative cs
    where
    countLetsInCaseAlternative (CaseAlternative _ r) = 
      either (concatMap (\(g, e1) -> findBindIdents g ++ findBindIdents e1)) findBindIdents r
  findBindIdents (Let _ bs e) = concatMap fn bs ++ findBindIdents e
    where
    fn (NonRec _ i e1) = i : findBindIdents e1
    fn (Rec as)        = foldl' (\acc ((_, i), e1) -> i : findBindIdents e1 ++ acc) [] as

spec :: Spec
spec = do
  context "generators" $ do
    specify "should generate Expr" $ property $ prop_exprDistribution
  context "runBindDeadCodeElimination" $ do
    specify "should reduce the depth of the tree" $ property $ withMaxSuccess 10000 prop_exprDepth
    specify "should reduce the number of let bindings" $ property $ withMaxSuccess 10000 prop_lets
    specify "should remove unused identifier" $ do
      let e :: Expr Ann
          e = Let ann
                [ NonRec ann (Ident "notUsed") (Literal ann (CharLiteral 'a'))
                , NonRec ann (Ident "used") (Literal ann (CharLiteral 'b'))
                ]
                (Var ann (Qualified Nothing (Ident "used")))
      case runBindDeadCodeElimination (NonRec ann (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` not . hasIdent (Ident "notUsed")
          bs `shouldSatisfy` hasIdent (Ident "used")
        _ -> return ()

    specify "should not remove transitive dependency" $ do
      let e :: Expr Ann
          e = Let ann
                [ NonRec ann (Ident "used") (Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "trDep"))))
                , NonRec ann (Ident "trDep") (Literal ann (CharLiteral 'a'))
                ]
                (Var ann (Qualified Nothing (Ident "used")))
      case runBindDeadCodeElimination (NonRec ann (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` hasIdent (Ident "trDep")
          bs `shouldSatisfy` hasIdent (Ident "used")
        _ -> return ()

    specify "should include all used recursive binds" $ do
      let e :: Expr Ann
          e = Let ann
                [ NonRec ann (Ident "entry") (Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "mutDep1"))))
                , Rec
                  [ ((ann, Ident "mutDep1"), Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "mutDep2"))))
                  , ((ann, Ident "mutDep2"), Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "mutDep1"))))
                  ]
                ]
                (App ann (Var ann (Qualified Nothing (Ident "entry"))) (Literal ann (CharLiteral 'a')))
      case runBindDeadCodeElimination (NonRec ann (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` hasIdent (Ident "entry")
          bs `shouldSatisfy` hasIdent (Ident "mutDep1")
          bs `shouldSatisfy` hasIdent (Ident "mutDep2")
        _ -> return ()

    specify "should dce case expressions" $ do
      let e :: Expr Ann
          e = Let ann
                [ NonRec ann (Ident "usedInExpr") (Literal ann (CharLiteral 'a'))
                , NonRec ann (Ident "notUsed") (Literal ann (CharLiteral 'a'))
                , NonRec ann (Ident "usedInGuard") (Literal ann (CharLiteral 'a'))
                , NonRec ann (Ident "usedInResult1") (Literal ann (CharLiteral 'a'))
                , NonRec ann (Ident "usedInResult2") (Literal ann (CharLiteral 'a'))
                ]
                (Case ann
                  [Var ann (Qualified Nothing (Ident "usedInExpr"))]
                  [ CaseAlternative
                      [NullBinder ann]
                      (Left
                        [ ( Var ann (Qualified Nothing (Ident "usedInGuard"))
                          , Var ann (Qualified Nothing (Ident "usedInResult1"))
                          )
                        ])
                  , CaseAlternative
                      [NullBinder ann]
                      (Right $ Var ann (Qualified Nothing (Ident "usedInResult2")))
                  ])
      case runBindDeadCodeElimination (NonRec ann (Ident "v") e) of
        NonRec _ _ (Let _ bs _) -> do
          bs `shouldSatisfy` hasIdent (Ident "usedInExpr")
          bs `shouldSatisfy` not . hasIdent (Ident "notUsed")
          bs `shouldSatisfy` hasIdent (Ident "usedInGuard")
          bs `shouldSatisfy` hasIdent (Ident "usedInResult1")
          bs `shouldSatisfy` hasIdent (Ident "usedInResult2")
        _ -> return ()

    specify "should not remove shadowed identifiers" $ do
      let e :: Expr Ann
          e = Let ann
                [ NonRec ann (Ident "shadow") (Literal ann (CharLiteral 'a'))
                , NonRec ann (Ident "sunny") (Literal ann (CharLiteral 'a'))
                ]
                $ Let ann
                  [ NonRec ann (Ident "shadow") (Literal ann (CharLiteral 'a')) ]
                  $ Literal ann
                    $ ObjectLiteral 
                      [ ( mkString "a", Var ann (Qualified Nothing (Ident "shadow")) )
                      , ( mkString "b", Var ann (Qualified Nothing (Ident "sunny")) )
                      ]
      case runBindDeadCodeElimination (NonRec ann (Ident "v") e) of
        NonRec _ _ (Let _ bs (Let _ cs _)) -> do
          bs `shouldSatisfy` hasIdent (Ident "sunny")
          bs `shouldSatisfy` not . hasIdent (Ident "shadow")
          cs `shouldSatisfy` hasIdent (Ident "shadow")
        _ -> undefined
