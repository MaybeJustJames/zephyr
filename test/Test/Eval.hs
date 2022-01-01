{-# LANGUAGE NumericUnderscores #-}

module Test.Eval (spec) where

import Prelude ()
import Prelude.Compat

import qualified Data.Map as Map

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn
import Language.PureScript.DCE
import qualified Language.PureScript.DCE.Constants as C
import Language.PureScript.Names
import Language.PureScript.PSString

import Language.PureScript.DCE.Utils (showExpr)

import Test.Hspec
import Test.HUnit (assertFailure)


ss :: SourceSpan
ss = SourceSpan "src/Test.purs" (SourcePos 0 0) (SourcePos 0 0)

ann :: Ann
ann = ssAnn ss

eq :: Qualified Ident
eq = Qualified (Just C.eqMod) (Ident "eq")

eqBoolean :: Qualified Ident
eqBoolean = Qualified (Just eqModName) (Ident "eqBoolean")

eqModName :: ModuleName
eqModName = ModuleName "Data.Eq"

mn :: ModuleName
mn = ModuleName "Test"

mp :: FilePath
mp = "src/Test.purs"

dceEvalExpr' :: Expr Ann -> [Module Ann] -> Expr Ann
dceEvalExpr' e mods = case evaluate ([testMod , eqMod , booleanMod , arrayMod, unsafeCoerceMod] ++ mods) of
    ((Module _ _ _ _ _ _ _ _ [NonRec _ _ e', _]) : _) -> e'
    _                                               -> error "not supported"
  where
  testMod = Module ss [] mn mp [] [] Map.empty []
    [ NonRec ann (Ident "v") e
    , NonRec ann (Ident "f")
        (Abs ann (Ident "x") (Var ann (Qualified Nothing (Ident "x"))))
    ]
  eqMod = Module ss [] C.eqMod "" [] [] Map.empty
    [ Ident "refEq" ]
    [ NonRec ann (Ident "eq")
        (Abs ann (Ident "dictEq")
          (Abs ann (Ident "x")
            (Abs ann (Ident "y")
              (Literal ann (BooleanLiteral True)))))
    , NonRec ann (Ident "eqBoolean")
        (App ann
          (Var ann (Qualified (Just C.eqMod) (Ident "Eq")))
          (Var ann (Qualified (Just C.eqMod) (Ident "refEq"))))
    , NonRec ann (Ident "Eq")
        (Abs ann (Ident "eq")
          (Literal ann (ObjectLiteral [(mkString "eq", Var ann (Qualified Nothing (Ident "eq")))])))
    ]
  booleanMod = Module ss [] (ModuleName "Data.Boolean") "" [] [] Map.empty []
    [ NonRec ann (Ident "otherwise") (Literal ann (BooleanLiteral True)) ]
  arrayMod = Module ss [] (ModuleName "Data.Array") ""
    [] [] Map.empty []
    [ NonRec ann (Ident "index")
        (Abs ann (Ident "as")
          (Abs ann (Ident "ix")
            (Literal ann (CharLiteral 'f'))))
    ]
  unsafeCoerceMod = Module ss [] C.unsafeCoerce ""
    [] [] Map.empty []
    [ NonRec ann (Ident "unsafeCoerce")
        (Abs ann (Ident "x")
          (Var ann (Qualified Nothing (Ident "x"))))
    ]

dceEvalExpr :: Expr Ann -> Expr Ann
dceEvalExpr e = dceEvalExpr' e []


-- TODO: need to generate valid `PSExpr`s.
{-
prop_eval :: PSExpr Ann -> Property
prop_eval (PSExpr g) =
  let d  = exprDepth g
      g' = dceEvalExpr g
      d' = exprDepth g'
  in
    collect (if d > 0 then 10 * (d' * 100 `div` (10 * d)) else 0)
    $ counterexample ("depth " ++ show d ++ " / " ++ show d' ++ "\n\t" ++ show g')
    $ (d' <= d)
-}


spec :: Spec
spec =
  context "evaluate" $ do
    -- specify "should evaluate" $ property $ withMaxSuccess 100_000 prop_eval
    specify "should simplify when comparing two literal values" $ do
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
        (Literal _ (CharLiteral 't')) -> return ()
        x -> assertFailure $ "unexepcted expression:\n" ++ showExpr x
        -- Left err -> assertFailure $ "compilation error: " ++ show err

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
       (Literal _ (CharLiteral 't')) -> return ()
       x -> assertFailure $ "unexepcted expression:\n" ++ showExpr x
       -- Left err -> assertFailure $ "compilation error: " ++ show err

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
        Let _ _ (Literal _ (CharLiteral 't')) -> return ()
        x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        -- Left err -> assertFailure $ "compilation error: " ++ show err

    specify "should not simplify application" $ do
      let -- f eqBoolean True True
          e :: Expr Ann
          e =
            App ann
              (App ann
                (App ann
                  (Var ann (Qualified (Just mn) (Ident "f")))
                  (Var ann eqBoolean))
                (Literal ann (BooleanLiteral True)))
              (Literal ann (BooleanLiteral True))
      case dceEvalExpr e of
        e' ->
          if showExpr e' /= showExpr e -- TODO! This is a dirty hack!
            then assertFailure $ "unexpected expression:\n" ++ showExpr e' ++ "\nexpected:\n" ++ showExpr e
            else return ()
        -- Left err -> assertFailure $ "compilation error: " ++ show err

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
                  , ( Var ann (Qualified (Just (ModuleName "Data.Boolean")) (Ident "otherwise"))
                    , (Literal ann (CharLiteral 'f'))
                    )
                  ])
              ]
      case dceEvalExpr e of
        (Case _
          [ Literal _ (BooleanLiteral True)]
          [ CaseAlternative
              [ VarBinder _ (Ident "x") ]
              (Left [ (Literal _ (BooleanLiteral True), Literal _ (CharLiteral 't')) ])
          ]
          ) -> return ()
        x   -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        -- Left err  -> assertFailure $ "compilation error: " ++ show err

    specify "should evaluate exported literal" $ do
      let um :: Module Ann
          um = Module ss []
            (ModuleName "Utils")
            "src/Utils.purs"
            []
            [Ident "isProduction"]
            Map.empty
            []
            [NonRec ann (Ident "isProduction") (Literal ann (BooleanLiteral True))]
          e :: Expr Ann
          e = Case ann
            [ Var ann (Qualified (Just (ModuleName "Utils")) (Ident "isProduction"))]
            [ CaseAlternative [LiteralBinder ann (BooleanLiteral True)] (Right (Literal ann (CharLiteral 't')))
            , CaseAlternative [LiteralBinder ann (BooleanLiteral False)] (Right (Literal ann (CharLiteral 'f')))
            ]
          mm :: Module Ann
          mm = Module
            ss
            []
            (ModuleName "Main")
            "src/Main.purs"
            []
            []
            Map.empty
            []
            [NonRec ann (Ident "main") e]
      -- TODO
      case evaluate [mm, um] of
        ((Module _ _ _ _ _ _ _ _ [NonRec _ (Ident "main") (Literal _ (CharLiteral 't'))]) : _) -> return ()
        r -> assertFailure $ "unexpected result:\n" ++ show r
        -- Left err -> assertFailure $ "compilation error: " ++ show err

    specify "should evaluate accessor expression" $ do
      let e :: Expr Ann
          e = (Accessor ann (mkString "a") (Literal ann (ObjectLiteral [(mkString "a", Literal ann (CharLiteral 't'))])))
      case dceEvalExpr e of
        (Literal _ (CharLiteral 't')) -> return ()
        x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        -- Left err -> assertFailure $ "compilation error: " ++ show err

    specify "should evaluate accessing array by index" $ do
      let e :: Expr Ann
          e = (App ann
                (App ann
                  (Var ann (Qualified (Just (ModuleName "Data.Array")) (Ident "index")))
                  (Literal ann (ArrayLiteral [Literal ann (CharLiteral 't')])))
                (Literal ann (NumericLiteral (Left 0))))
      case dceEvalExpr e of
        (App _ (Var _ (Qualified (Just (ModuleName "Data.Maybe")) (Ident "Just"))) (Literal _ (CharLiteral 't'))) -> return ()
        x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
        -- Left err -> assertFailure $ "compilation error: " ++ show err

    context "context stack" $ do
      specify "nested let bindings" $ do
        let -- let a = 'a'
            -- in let a = 'b'
            --    in a
            e :: Expr Ann
            e = Let ann [ NonRec ann (Ident "a") (Literal ann (CharLiteral 'a')) ]
                  (Let ann [ NonRec ann (Ident "a") (Literal ann (CharLiteral 'b')) ]
                    (Var ann (Qualified Nothing (Ident "a"))))
        case dceEvalExpr e of
          Let _ _ (Let _ _ (Literal _ (CharLiteral 'b'))) -> return ()
          x -> assertFailure $ "unexpected expression:\n" ++ showExpr x
          -- Left err -> assertFailure $ "compilation error: " ++ show err

    context "Var inlining" $ do
      let oModName = ModuleName "Other"
          oMod = Module ss [] oModName "" [] [] Map.empty []
            [ NonRec ann (Ident "o") $ Literal ann (ObjectLiteral [(mkString "a", Var ann (Qualified (Just C.eqMod) (Ident "eq"))) ])
            , NonRec ann (Ident "a") $ Literal ann (ArrayLiteral [ Var ann (Qualified (Just C.eqMod) (Ident "eq")) ])
            , NonRec ann (Ident "s") $ Literal ann (StringLiteral (mkString "very-long-string"))
            , NonRec ann (Ident "b") $ Literal ann (BooleanLiteral True)
            , NonRec ann (Ident "c") $ Literal ann (CharLiteral 'a')
            , NonRec ann (Ident "n") $ Literal ann (NumericLiteral (Left 0))
            ]
      specify "should not inline Var linking to an object literal" $ do
        let e :: Expr Ann
            e = Var ann (Qualified (Just oModName) (Ident "o"))
        case dceEvalExpr' e [oMod] of
          Var{} -> return ()
          e' -> assertFailure $ "unexpected expression: " ++ showExpr e'
          -- Left err -> assertFailure $ "compilation error: " ++ show err

      specify "should not inline Var linking to an array literal" $ do
        let e :: Expr Ann
            e = Var ann (Qualified (Just oModName) (Ident "a"))
        case dceEvalExpr' e [oMod] of
          Var{} -> return ()
          e' -> assertFailure $ "unexpected expression: " ++ showExpr e'
          -- Left err -> assertFailure $ "compilation error: " ++ show err

      specify "should not inline Var linking to a string literal" $ do
        let e :: Expr Ann
            e = Var ann (Qualified (Just oModName) (Ident "s"))
        case dceEvalExpr' e [oMod] of
          Var{} -> return ()
          e' -> assertFailure $ "unexpected expression: " ++ showExpr e'
          -- Left err -> assertFailure $ "compilation error: " ++ show err

      specify "should inline Var lining to a boolean literal" $ do
        let e :: Expr Ann
            e = Var ann (Qualified (Just oModName) (Ident "b"))
        case dceEvalExpr' e [oMod] of
          (Literal _ (BooleanLiteral{})) -> return ()
          e' -> assertFailure $ "wront expression: " ++ showExpr e'
          -- Left err -> assertFailure $ "compilation error: " ++ show err

      specify "should inline Var lining to a char literal" $ do
        let e :: Expr Ann
            e = Var ann (Qualified (Just oModName) (Ident "c"))
        case dceEvalExpr' e [oMod] of
          (Literal _ (CharLiteral{})) -> return ()
          e' -> assertFailure $ "wront expression: " ++ showExpr e'
          -- Left err -> assertFailure $ "compilation error: " ++ show err

      specify "should inline Var lining to a numeric literal" $ do
        let e :: Expr Ann
            e = Var ann (Qualified (Just oModName) (Ident "n"))
        case dceEvalExpr' e [oMod] of
          (Literal _ (NumericLiteral{})) -> return ()
          e' -> assertFailure $ "wront expression: " ++ showExpr e'
          -- Left err -> assertFailure $ "compilation error: " ++ show err
