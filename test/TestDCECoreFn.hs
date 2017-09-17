module TestDCECoreFn (main) where

import Prelude ()
import Prelude.Compat

import Data.List (concatMap)

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn
import Language.PureScript.DCE
import Language.PureScript.Names
import Language.PureScript.PSString

import Test.Hspec

main :: IO ()
main = hspec spec

getNames :: Bind a -> [Ident]
getNames (NonRec _ i _) = [i]
getNames (Rec l) = (\((_, i), _) -> i) `map` l

hasIdent :: Ident -> [Bind Ann] -> Bool
hasIdent i = (i `elem`) . concatMap getNames

ann = ssAnn (SourceSpan "src/Test.purs" (SourcePos 0 0) (SourcePos 0 0))

spec :: Spec
spec =
  context "dceExpr" $ do
    specify "should remove unused identifier" $ do
      let e :: Expr Ann
          e = Let ann
                [ NonRec ann (Ident "notUsed") (Literal ann (CharLiteral 'a'))
                , NonRec ann (Ident "used") (Literal ann (CharLiteral 'b'))
                ]
                (Var ann (Qualified Nothing (Ident "used")))
      case dceExpr (NonRec ann (Ident "v") e) of
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
      case dceExpr (NonRec ann (Ident "v") e) of
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
      case dceExpr (NonRec ann (Ident "v") e) of
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
      case dceExpr (NonRec ann (Ident "v") e) of
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
      case dceExpr (NonRec ann (Ident "v") e) of
        NonRec _ _ (Let _ bs (Let _ cs _)) -> do
          bs `shouldSatisfy` hasIdent (Ident "sunny")
          bs `shouldSatisfy` not . hasIdent (Ident "shadow")
          cs `shouldSatisfy` hasIdent (Ident "shadow")
        _ -> undefined
