{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Test.Generators where

import Data.List (foldl')
import Data.String (IsString (..))
import Test.QuickCheck

import Language.PureScript.Names (Ident (..), ModuleName (..), ProperName (..), Qualified (..))
import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos (SourceSpan (..), SourcePos (..))
import Language.PureScript.AST (Literal (..))
import Language.PureScript.CoreFn (Ann, Bind (..), Binder (..), CaseAlternative (..), Expr (..), Guard, ssAnn)

import qualified Language.PureScript.DCE.Constants as C

ann :: Ann
ann = ssAnn (SourceSpan "src/Test.purs" (SourcePos 0 0) (SourcePos 0 0))

genPSString :: Gen PSString
genPSString = fromString <$> elements
  ["a", "b", "c", "d", "value0"]

genProperName :: Gen (ProperName a)
genProperName = ProperName <$> elements
  ["A", "B", "C", "D", "E"]

genIdent :: Gen Ident
genIdent = Ident <$> elements
  ["value0", "value1", "value2"]

unusedIdents :: [Ident]
unusedIdents =
  Ident <$> ["u1", "u2", "u3", "u4", "u5"]

genUnusedIdent :: Gen Ident
genUnusedIdent = elements unusedIdents

genModuleName :: Gen ModuleName
genModuleName = elements
  [ ModuleName "Data.Eq"
  , ModuleName "Data.Array"
  , ModuleName "Data.Maybe"
  , C.semigroup
  , C.unsafeCoerce
  , C.unit
  , C.semiring
  ]

genQualifiedIdent :: Gen (Qualified Ident)
genQualifiedIdent = oneof
  [ Qualified <$> liftArbitrary genModuleName <*> genIdent
  , return (Qualified (Just C.unit) (Ident "unit"))
  , return (Qualified (Just C.semiring) (Ident "add"))
  , return (Qualified (Just C.semiring) (Ident "semiringInt"))
  , return (Qualified (Just C.semiring) (Ident "semiringUnit"))
  , return (Qualified (Just C.maybeMod) (Ident "Just"))
  , return (Qualified (Just C.eqMod) (Ident "eq"))
  , return (Qualified (Just C.ring) (Ident "negate"))
  , return (Qualified (Just C.ring) (Ident "ringNumber"))
  , return (Qualified (Just C.ring) (Ident "unitRing"))
  ]

genQualified :: Gen a -> Gen (Qualified a)
genQualified gen = Qualified <$> liftArbitrary genModuleName <*> gen

genLiteral :: Gen (Literal (Expr Ann))
genLiteral = oneof
  [ NumericLiteral <$> arbitrary
  , StringLiteral  <$> genPSString
  , CharLiteral    <$> arbitrary
  , BooleanLiteral <$> arbitrary
  , ArrayLiteral . map unPSExpr <$> arbitrary
  , ObjectLiteral . map (\(k, v) -> (fromString k, unPSExpr v)) <$> arbitrary
  ]

genLiteral' :: Gen (Expr Ann)
genLiteral' = oneof
  [ Literal ann . NumericLiteral <$> arbitrary
  , Literal ann . StringLiteral <$> genPSString
  , Literal ann . BooleanLiteral <$> arbitrary
  , Literal ann . CharLiteral <$> arbitrary
  ]

-- TODO: this generator is very frigile with size and at times can generate
-- huge data.  We use size 4.  In particual it is very sensitive on the
-- frequency of generating let expressions.
genExpr :: Gen (Expr Ann)
genExpr = sized go
  where
    go :: Int -> Gen (Expr Ann)
    go 0 = oneof
      [ Literal ann <$> genLiteral
      , Constructor ann <$> genProperName <*> genProperName <*> listOf genIdent
      , Var ann <$> genQualifiedIdent
      ]
    go n = frequency
      [ (3, Literal ann <$> genLiteral)
      , (3, Constructor ann <$> genProperName <*> genProperName <*> listOf genIdent)
      , (3, Var ann <$> genQualifiedIdent)
      , (4, Accessor ann <$> genPSString <*> scale succ genExpr)
      , (1, ObjectUpdate ann <$> genExpr <*> resize (max 3 (n - 1)) (listOf ((,) <$> genPSString <*> genExpr)))
      , (2, Abs ann <$> genIdent <*> scale succ genExpr)
      , (1, App ann <$> scale succ genExpr <*> scale succ genExpr)
      , (4, genApp)
      , (1, Case ann <$> resize (max 3 (n `div` 2)) (listOf genExpr) <*> resize (max 2 (n `div` 2)) (listOf (scale succ genCaseAlternative)))
      , (2, Let ann <$> listOf genBind <*> scale (`div` 2) genExpr)
      ]

genCaseAlternative :: Gen (CaseAlternative Ann)
genCaseAlternative = sized $ \n ->
  CaseAlternative <$> vectorOf n genBinder <*> genCaseAlternativeResult n
  where
  genCaseAlternativeResult :: Int -> Gen (Either [(Guard Ann, Expr Ann)] (Expr Ann))
  genCaseAlternativeResult n = oneof
    [ Left  <$> vectorOf n ((,) <$> resize n genExpr <*> resize n genExpr)
    , Right <$> resize n genExpr
    ]

newtype PSBinder = PSBinder { unPSBinder :: Binder Ann }
  deriving Show

genBinder :: Gen (Binder Ann)
genBinder = sized go
  where
    go :: Int -> Gen (Binder Ann)
    go 0 = oneof
      [ return $ NullBinder ann
      , VarBinder ann <$> genIdent
      ]
    go _ = frequency
      [ (1, return $ NullBinder ann)
      , (2, LiteralBinder ann . ArrayLiteral  <$> listOf (scale succ genBinder))
      , (2, LiteralBinder ann . ObjectLiteral <$> listOf ((,) <$> genPSString <*> (scale succ genBinder)))
      , (3, ConstructorBinder ann <$> genQualified genProperName <*> genQualified genProperName <*> listOf (scale succ genBinder))
      , (3, NamedBinder ann <$> genIdent <*> scale succ genBinder)
      ]

instance Arbitrary PSBinder where
  arbitrary =  PSBinder <$> resize 5 genBinder

  shrink (PSBinder (LiteralBinder _ (ArrayLiteral bs))) =
    (PSBinder . LiteralBinder ann . ArrayLiteral . map unPSBinder
    <$> (shrinkList shrink (PSBinder <$> bs)))
    ++ map PSBinder bs
  shrink (PSBinder (LiteralBinder _ (ObjectLiteral o))) =
    (PSBinder . LiteralBinder ann . ObjectLiteral
    <$> shrinkList (\(n, b) -> (n,) . unPSBinder <$> shrink (PSBinder b)) o)
    ++ map (PSBinder . snd) o
  shrink (PSBinder (ConstructorBinder _ tn cn bs)) =
    (PSBinder . ConstructorBinder ann tn cn . map unPSBinder
    <$> (shrinkList shrink (PSBinder <$> bs)))
    ++ map PSBinder bs
  shrink (PSBinder (NamedBinder _ n b)) =
    PSBinder b
    : (PSBinder . NamedBinder ann n . unPSBinder <$> shrink (PSBinder b))
  shrink _ = []

prop_binderDistribution :: PSBinder -> Property
prop_binderDistribution (PSBinder c) =
    classify True (show . depth $ c)
  $ tabulate "Binders" (cls c) True
  where
  cls NullBinder{}                 = ["NullBinder"]
  cls LiteralBinder{}              = ["LiteralBinder"]
  cls VarBinder{}                  = ["VarBinder"]
  cls (ConstructorBinder _ _ _ bs) = "ConstructorBinder" : concatMap cls bs
  cls (NamedBinder _ _ b)          = "NamedBinder" : cls b

  depth :: Binder a -> Int
  depth NullBinder{}                        = 1
  depth (LiteralBinder _ (ArrayLiteral bs)) = foldr (\b x -> depth b `max` x) 1 bs + 1
  depth (LiteralBinder _ (ObjectLiteral o)) = foldr (\(_, b) x -> depth b `max` x) 0 o + 1
  depth LiteralBinder{}                     = 1
  depth VarBinder{}                         = 1
  depth (ConstructorBinder _ _ _ bs)        = foldr (\b x -> depth b `max` x) 1 bs + 1
  depth (NamedBinder _ _ b)                 = depth b

genBind :: Gen (Bind Ann)
genBind = frequency
  [ (3, NonRec ann <$> gen  <*> (scale (`div` 2) genExpr))
  , (1, Rec <$> listOf ((\i e -> ((ann, i), e)) <$> gen <*> (scale (`div` 2) genExpr)))
  ]
  where
  gen = frequency [(3, genIdent), (2, genUnusedIdent)]

newtype PSExpr a = PSExpr { unPSExpr :: Expr a }
  deriving Show

-- Generate simple curried functions
genApp :: Gen (Expr Ann)
genApp =
  App ann
    <$> frequency
        [ (1, genApp)
        , (2, Var ann <$> genQualifiedIdent)
        ]
    <*> frequency
        [ (2, Var ann <$> genQualifiedIdent)
        , (3, genLiteral')
        ]

instance Arbitrary (PSExpr Ann) where
  arbitrary = PSExpr <$> resize 4 genExpr

  shrink (PSExpr expr) = map PSExpr $ go expr
    where
    go :: Expr Ann -> [Expr Ann]
    go (Literal ann' (ArrayLiteral es)) =
      (Literal ann' . ArrayLiteral <$> shrinkList shrinkExpr es)
      ++ es
    go (Literal ann' (ObjectLiteral o)) =
      (Literal ann' . ObjectLiteral
      <$> shrinkList (\(n, e) -> (n,) <$> shrinkExpr e) o)
      ++ map snd o
    go (Accessor ann' n e) =
      e : (Accessor ann' n <$> shrinkExpr e)
    go (ObjectUpdate ann' e es) =
      e : map snd es
      ++ [ ObjectUpdate ann' e' es
         | e'  <- shrinkExpr e
         ]
      ++ [ ObjectUpdate ann' e es'
         | es' <- shrinkList (\(n, f) -> map (n,) $ shrinkExpr f) es
         ]
    go (Abs ann' n e) =
      let es = shrinkExpr e
      in e : es ++ map (Abs ann' n) es
    go (App ann' e f) =
      e : f : [ App ann' e' f  | e' <- shrinkExpr e ]
          ++  [ App ann' e  f' | f' <- shrinkExpr f ]
    go Var{} = []
    go (Case ann' es cs) =
         [ Case ann' es cs'
         | cs' <- shrinkList shrinkCaseAlternative cs
         ]
      ++ [ Case ann' es' cs
         | es' <- shrinkList shrinkExpr es
         ]
      ++ es
      ++ concatMap
          (\(CaseAlternative _ r) ->
            either
              (\es' -> map fst es' ++ map snd es')
              (\e' -> [e'])
              r
          )
          cs
    go (Let ann' bs e) =
      e : [ Let ann' bs  e' | e'  <- shrinkExpr e ]
       ++ [ Let ann' bs' e  | bs' <- shrinkList shrinkBind bs ]
    go _ = []

    shrinkCaseAlternative :: CaseAlternative Ann -> [CaseAlternative Ann]
    shrinkCaseAlternative (CaseAlternative bs r) =
         [ CaseAlternative bs r'
         | r' <-
             case r of
               Right e  -> Right <$> shrinkExpr e
               Left es' -> Left  <$> shrinkList
                                       (\(g, f) -> [(g, f') | f' <- shrinkExpr f]
                                                ++ [(g', f) | g' <- shrinkExpr g]) es'
         ]
      ++ [ CaseAlternative bs' r
         | bs' <- shrinkList (\x -> [x]) bs
         ]

shrinkExpr :: Expr Ann -> [Expr Ann]
shrinkExpr = map unPSExpr . shrink . PSExpr

shrinkBind :: Bind Ann -> [Bind Ann]
shrinkBind (NonRec ann' n e) = NonRec ann' n <$> shrinkExpr e
shrinkBind (Rec as) = Rec <$> shrinkList (\(x, e) -> map (x,) $ shrinkExpr e) as

exprDepth :: Expr a -> Int
exprDepth (Literal _ (ArrayLiteral es)) = foldl' (\x e -> exprDepth e `max` x) 1 es + 1
exprDepth (Literal _ (ObjectLiteral o)) = foldl' (\x (_, e) -> exprDepth e `max` x) 1 o + 1
exprDepth (Literal{})   = 1
exprDepth Constructor{} = 1
exprDepth (Accessor _ _ e) = 1 + exprDepth e
exprDepth (ObjectUpdate _ e es) = 1 + exprDepth e + foldl' (\x (_, f) -> exprDepth f `max` x) 1 es
exprDepth (Abs _ _ e) = 1 + exprDepth e
exprDepth (App _ e f) = 1 + exprDepth e `max` exprDepth f
exprDepth Var{}       = 1
exprDepth (Case _ es cs) = 1 + foldl' (\x f -> exprDepth f `max` x) cdepth es
  where
    cdepth = foldl' (\x (CaseAlternative _ r) -> either (foldl' (\y (g, e) -> exprDepth g `max` exprDepth e `max` y) 1) exprDepth r `max` x) 1 cs
exprDepth (Let _ bs e) = 1 + exprDepth e `max` foldl' (\x b -> binderExprDepth b `max` x) 0 bs
  where
    binderExprDepth :: Bind a -> Int
    binderExprDepth (NonRec _ _ e') = exprDepth e'
    binderExprDepth (Rec es') = foldl' (\x (_, e') -> x `max` exprDepth e') 0 es'

prop_exprDistribution :: PSExpr Ann -> Property
prop_exprDistribution (PSExpr e) =
    collect d
  $ tabulate "classify expressions" (cls e) True
  where
  d = exprDepth' e
  cls :: Expr a -> [String]
  cls Literal{}      = ["Literal"]
  cls Constructor{}  = ["Constructor"]
  cls Accessor{}     = ["Accessor"]
  cls ObjectUpdate{} = ["ObjectUpdate"]
  cls Abs{}          = ["Abs"]
  cls App{}          = ["App"]
  cls Var{}          = ["Var"]
  cls (Case _ _ cs)  = "Case" : foldl' (\x c -> clsCaseAlternative c ++ x) [] cs
    where
    clsCaseAlternative (CaseAlternative {caseAlternativeResult}) =
      either (foldl' (\x (g, f) -> cls g ++ cls f ++ x) []) cls caseAlternativeResult
  cls Let{}          = ["Let"]

  exprDepth' expr = case exprDepth expr of
    n | n < 10     -> n
      | n < 100   -> 10 * (n `div` 10)
      | n < 1000  -> 25 * (n `div` 25)
      | otherwise -> 100 * (n `div` 100)
