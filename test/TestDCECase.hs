{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestDCECase (main) where

import Prelude ()
import Prelude.Compat

import Data.List (concatMap)

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn
import Language.PureScript.DCE
import Language.PureScript.Names
import Language.PureScript.PSString

import Language.PureScript.DCE.Utils (showExpr)

import Test.Hspec
import Test.HUnit (assertFailure)

main :: IO ()
main = hspec spec

ann = ssAnn (SourceSpan "src/Test.purs" (SourcePos 0 0) (SourcePos 0 0))

spec :: Spec
spec =
  context "dceCase" $ do
    let eqMod = ModuleName [ProperName "Data", ProperName "Eq"]
        eq = Qualified (Just eqMod) (Ident "eq")
        eqBoolean  = Qualified (Just eqMod) (Ident "eqBoolean")
        mn = ModuleName [ProperName "Test"]
        mp = "src/Test.purs"

        dceCaseExpr :: Expr Ann -> Expr Ann
        dceCaseExpr e = case dceCase [Module [] mn mp [] [] [] [NonRec ann (Ident "v") e]] of [Module _ _ _ _ _ _ [NonRec _ _ e']] -> e'

    specify "should simplify if when comparing two literal values" $ do
      let v :: Expr Ann
          v =
            App ann
              (App ann
                (App ann
                  (Var ann eq)
                  (Var ann eqBoolean))
                (Literal ann (BooleanLiteral True)))
              (Literal ann (BooleanLiteral True))
          e :: Expr Ann
          e = Case ann [v]
            [ CaseAlternative
                [ LiteralBinder ann (BooleanLiteral True) ]
                (Right (Literal ann (CharLiteral 't')))
            , CaseAlternative
                [ LiteralBinder ann (BooleanLiteral False) ]
                (Right (Literal ann (CharLiteral 'f')))
            ]
      case dceCaseExpr e of
        (Literal ann (CharLiteral 't')) -> return ()
        x -> assertFailure $ "unexepcted expression:\n" ++ showExpr x

    specify "should simplify `if true`" $ do
      let e :: Expr Ann
          e = Case ann [Literal ann (BooleanLiteral True)]
            [ CaseAlternative
                [ LiteralBinder ann (BooleanLiteral True) ]
                (Right (Literal ann (CharLiteral 't')))
            , CaseAlternative
                [ LiteralBinder ann (BooleanLiteral False) ]
                (Right (Literal ann (CharLiteral 'f')))
            ]
      case dceCaseExpr e of
       (Literal ann (CharLiteral 't')) -> return ()
       x -> assertFailure $ "unexepcted expression:\n" ++ showExpr x

    specify "should simplify case when comparing two literal values" $ do
      let v :: Expr Ann
          v =
            App ann
              (App ann
                (App ann
                  (Var ann eq)
                  (Var ann eqBoolean))
                (Literal ann (BooleanLiteral True)))
              (Literal ann (BooleanLiteral True))
          e :: Expr Ann
          e = Let ann [NonRec ann (Ident "v") v]
                (Case ann [Var ann (Qualified Nothing (Ident "v"))]
                  [ CaseAlternative
                      [ LiteralBinder ann (BooleanLiteral True) ]
                      (Right (Literal ann (CharLiteral 't')))
                  , CaseAlternative
                      [ LiteralBinder ann (BooleanLiteral False) ]
                      (Right (Literal ann (CharLiteral 'f')))
                  ])
      case dceCaseExpr e of
        (Let _ [NonRec _ (Ident "v") _] (Literal _ (CharLiteral 't'))) -> return ()
        x -> assertFailure $ "unexpected expression:\n" ++ showExpr x

    specify "should not simplify application" $ do
      let v :: Expr Ann
          v =
            App ann
              (App ann
                (App ann
                  (Var ann (Qualified (Just mn) (Ident "eq")))
                  (Var ann eqBoolean))
                (Literal ann (BooleanLiteral True)))
              (Literal ann (BooleanLiteral True))
          e :: Expr Ann
          e = Let ann [NonRec ann (Ident "v") v]
                (Case ann [Var ann (Qualified Nothing (Ident "v"))]
                  [ CaseAlternative
                      [ LiteralBinder ann (BooleanLiteral True) ]
                      (Right (Literal ann (CharLiteral 't')))
                  , CaseAlternative
                      [ LiteralBinder ann (BooleanLiteral False) ]
                      (Right (Literal ann (CharLiteral 'f')))
                  ])
      let e' = dceCaseExpr e
      if showExpr e' /= showExpr e -- dirty
        then assertFailure $ "unexpected expression:\n" ++ showExpr e' ++ "\nexpected:\n" ++ showExpr e
        else return ()

    specify "eval guards" $ do
      let e :: Expr Ann
          e = Case ann [Literal ann (BooleanLiteral True)]
            [ CaseAlternative
              [ VarBinder ann (Ident "x") ]
                (Left
                  [ (App ann
                      (App ann
                        (App ann
                          (Var ann eq)
                          (Var ann eqBoolean))
                        (Var ann (Qualified Nothing (Ident "x"))))
                      (Literal ann (BooleanLiteral True))
                    , Literal ann (CharLiteral 't'))
                  , ( Var ann (Qualified (Just (ModuleName [ProperName "Data", ProperName "Boolean"])) (Ident "otherwise"))
                    , (Literal ann (CharLiteral 'f'))
                    )
                  ])
              ]
      case dceCaseExpr e of
        (Case _
          [ Literal _ (BooleanLiteral True)]
          [ CaseAlternative
              [ VarBinder _ (Ident "x") ]
              (Left [ (Literal _ (BooleanLiteral True), Literal _ (CharLiteral 't')) ])
          ]
          ) -> return ()
        x   -> assertFailure $ "unexpected expression:\n" ++ showExpr x

    specify "should evaluate exported literal" $ do
      let um :: ModuleT () Ann
          um = Module []
            (ModuleName [ProperName "Utils"])
            "src/Utils.purs"
            []
            [Ident "isProduction"]
            []
            [NonRec ann (Ident "isProduction") (Literal ann (BooleanLiteral True))]
          e :: Expr Ann
          e = Case ann
            [Var ann (Qualified (Just (ModuleName [ProperName "Utils"])) (Ident "isProduction"))]
            [ CaseAlternative [LiteralBinder ann (BooleanLiteral True)] (Right (Literal ann (CharLiteral 't')))
            , CaseAlternative [LiteralBinder ann (BooleanLiteral False)] (Right (Literal ann (CharLiteral 'f')))
            ]
          mm :: ModuleT () Ann
          mm = Module
            []
            (ModuleName [ProperName "Main"])
            "src/Main.purs"
            []
            []
            []
            [NonRec ann (Ident "main") e]
      case dceCase [mm, um] of
        (Module _ _ _ _ _ _ [NonRec _ (Ident "main") (Literal _ (CharLiteral 't'))]) : _ -> return ()
        r -> assertFailure $ "unexpected result:\n" ++ show r

    specify "should evaluate accessor expression" $ do
      let e :: Expr Ann
          e = (Accessor ann (mkString "a") (Literal ann (ObjectLiteral [(mkString "a", Literal ann (CharLiteral 't'))])))
      case dceCaseExpr e of
        (Literal _ (CharLiteral 't')) -> return ()
        x -> assertFailure $ "unexpected expression:\n" ++ showExpr x

    specify "should evaluate accessing array by index" $ do
      let e :: Expr Ann
          e = (App ann
                (App ann
                  (Var ann (Qualified (Just (ModuleName [ProperName "Data", ProperName "Array"])) (Ident "index")))
                  (Literal ann (ArrayLiteral [Literal ann (CharLiteral 't')])))
                (Literal ann (NumericLiteral (Left 0))))
      case dceCaseExpr e of
        (Literal _ (CharLiteral 't')) -> return ()
        x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
