module TestDCEEval (main) where

import Prelude ()
import Prelude.Compat

import Control.Monad.Writer

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

ann :: Ann
ann = ssAnn (SourceSpan "src/Test.purs" (SourcePos 0 0) (SourcePos 0 0))

spec :: Spec
spec =
  context "dceEval" $ do
    let eqModName = ModuleName [ProperName "Data", ProperName "Eq"]
        eq = Qualified (Just eqModName) (Ident "eq")
        eqBoolean  = Qualified (Just eqModName) (Ident "eqBoolean")
        eqMod = Module [] eqModName "" [] []
          [(Ident "refEq", ())]
          [ NonRec ann (Ident "eq")  
              (Abs ann (Ident "dictEq")
                (Abs ann (Ident "x")
                  (Abs ann (Ident "y")
                    (Literal ann (BooleanLiteral True)))))
          , NonRec ann (Ident "eqBoolean")
              (App ann
                (Var ann (Qualified (Just eqModName) (Ident "Eq")))
                (Var ann (Qualified (Just eqModName) (Ident "refEq"))))
          , NonRec ann (Ident "Eq")
              (Abs ann (Ident "eq")
                (Literal ann (ObjectLiteral [(mkString "eq", Var ann (Qualified Nothing (Ident "eq")))])))
          ]
        booleanMod = Module [] (ModuleName [ProperName "Data", ProperName "Boolean"]) "" [] [] []
          [ NonRec ann (Ident "otherwise") (Literal ann (BooleanLiteral True)) ]
        arrayMod = Module [] (ModuleName [ProperName "Data", ProperName "Array"]) ""
          [] [] []
          [ NonRec ann (Ident "index")
              (Abs ann (Ident "as")
                (Abs ann (Ident "ix")
                  (Literal ann (CharLiteral 'f'))))
          ]

        testMod e = Module [] mn mp [] [] []
          [ NonRec ann (Ident "v") e
          , NonRec ann (Ident "f")
              (Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "x"))))
          ]
          
        mn = ModuleName [ProperName "Test"]
        mp = "src/Test.purs"

        dceEvalExpr :: Expr Ann -> Either (DCEError 'Error) (Expr Ann)
        dceEvalExpr e = case runWriterT $ dceEval [testMod e , eqMod , booleanMod , arrayMod] of
          Right (((Module _ _ _ _ _ _ [NonRec _ _ e', _]): _), _) -> Right e'
          Right _   -> undefined
          Left err  -> Left err

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
      case dceEvalExpr e of
        Right (Literal _ (CharLiteral 't')) -> return ()
        Right x -> assertFailure $ "unexepcted expression:\n" ++ showExpr x
        Left err -> assertFailure $ "compilation error: " ++ show err

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
      case dceEvalExpr e of
       Right (Literal _ (CharLiteral 't')) -> return ()
       Right x -> assertFailure $ "unexepcted expression:\n" ++ showExpr x
       Left err -> assertFailure $ "compilation error: " ++ show err

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
      case dceEvalExpr e of
        Right (Let _ [NonRec _ (Ident "v") _] (Literal _ (CharLiteral 't'))) -> return ()
        Right x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        Left err -> assertFailure $ "compilation error: " ++ show err

    specify "should not simplify application" $ do
      let v :: Expr Ann
          v =
            App ann
              (App ann
                (App ann
                  (Var ann (Qualified (Just mn) (Ident "f")))
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
      case dceEvalExpr e of
        Right e' ->
          if showExpr e' /= showExpr e -- dirty
            then assertFailure $ "unexpected expression:\n" ++ showExpr e' ++ "\nexpected:\n" ++ showExpr e
            else return ()
        Left err -> assertFailure $ "compilation error: " ++ show err

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
      case dceEvalExpr e of
        Right (Case _
          [ Literal _ (BooleanLiteral True)]
          [ CaseAlternative
              [ VarBinder _ (Ident "x") ]
              (Left [ (Literal _ (BooleanLiteral True), Literal _ (CharLiteral 't')) ])
          ]
          ) -> return ()
        Right x   -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        Left err  -> assertFailure $ "compilation error: " ++ show err

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
            [ Var ann (Qualified (Just (ModuleName [ProperName "Utils"])) (Ident "isProduction"))]
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
      case runWriterT $ dceEval [mm, um] of
        Right (((Module _ _ _ _ _ _ [NonRec _ (Ident "main") (Literal _ (CharLiteral 't'))]) : _), _) -> return ()
        Right r -> assertFailure $ "unexpected result:\n" ++ show r
        Left err -> assertFailure $ "compilation error: " ++ show err

    specify "should evaluate accessor expression" $ do
      let e :: Expr Ann
          e = (Accessor ann (mkString "a") (Literal ann (ObjectLiteral [(mkString "a", Literal ann (CharLiteral 't'))])))
      case dceEvalExpr e of
        Right (Literal _ (CharLiteral 't')) -> return ()
        Right x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        Left err -> assertFailure $ "compilation error: " ++ show err

    specify "should evaluate accessing array by index" $ do
      let e :: Expr Ann
          e = (App ann
                (App ann
                  (Var ann (Qualified (Just (ModuleName [ProperName "Data", ProperName "Array"])) (Ident "index")))
                  (Literal ann (ArrayLiteral [Literal ann (CharLiteral 't')])))
                (Literal ann (NumericLiteral (Left 0))))
      case dceEvalExpr e of
        Right (App _ (Var _ (Qualified (Just (ModuleName [ProperName "Data", ProperName "Maybe"])) (Ident "Just"))) (Literal _ (CharLiteral 't'))) -> return ()
        Right x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        Left err -> assertFailure $ "compilation error: " ++ show err

    context "context stack" $ do
      specify "let and case binders" $ do
        let e :: Expr Ann
            e = Let ann [ NonRec ann (Ident "v") (Literal ann (CharLiteral 'v')) ]
                  (Case ann
                    [ Literal ann (CharLiteral 't') ]
                    [ CaseAlternative
                        [ VarBinder ann (Ident "v") ]
                        (Right (Var ann (Qualified Nothing (Ident "v"))))
                    ]
                  )
        case dceEvalExpr e of
          Right (Let _ _ (Case _ _ [ CaseAlternative _ (Right (Literal _ (CharLiteral 't'))) ])) -> return ()
          Right x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
          Left err -> assertFailure $ "compilation error: " ++ show err

      specify "nested let bindings" $ do
        let e :: Expr Ann
            e = Let ann [ NonRec ann (Ident "a") (Literal ann (CharLiteral 'a')) ]
                  (Let ann [ NonRec ann (Ident "a") (Literal ann (CharLiteral 'b')) ]
                    (Var ann (Qualified Nothing (Ident "a"))))
        case dceEvalExpr e of
          Right (Let _ _ (Let _ _ (Literal _ (CharLiteral 'b')))) -> return ()
          Right x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
          Left err -> assertFailure $ "compilation error: " ++ show err
