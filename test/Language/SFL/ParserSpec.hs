{-# LANGUAGE TypeFamilies #-}
module Language.SFL.ParserSpec where

import           Data.Either
import           Language.SFL.Parser
import           Language.SFL.Printer
import           Language.SFL.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           TestUtil

instance Typed () where
  typeOf () = StringType
instance RecordField () where
  type RecordOf () = ()
  toRecordId () = "test"
  fromRecordId "test" = Just ()
  fromRecordId _      = Nothing
  recordValue () () = StringV "()"

functions@[tf, tf2, plus, mult, lessThan, minus] =
  [ Function "tf" ([NumberType, NumberType], NumberType) (\[NumberV x] -> NumberV x) 7
  , Function "tf2" ([NumberType, NumberType], NumberType) (\[NumberV x, NumberV y] -> NumberV x) 7
  , Function "+" ([NumberType, NumberType], NumberType) (\[NumberV x, NumberV y] -> NumberV $ x + y) 6
  , Function "*" ([NumberType, NumberType], NumberType) (\[NumberV x, NumberV y] -> NumberV $ x * y) 7
  , Function "<" ([NumberType, NumberType], BoolType) (\[NumberV x, NumberV y] -> BoolV $ x < y) 5
  , Function "-" ([NumberType, NumberType], BoolType) (\[NumberV x, NumberV y] -> NumberV $ x - y) 5
  ]

parse'' = flip parse' (SflParseState functions :: SflParseState ())

instance Arbitrary Literal where
  arbitrary = do
    n <- elements [0..1]
    case n of
      0 -> StringL <$> pure "test"
      1 -> NumberL <$> arbitrary
      _ -> error "wtf"

instance (Arbitrary a, RecordField a) => Arbitrary (Expr a) where
  arbitrary = frequency [(1, record'), (1, literal'), (5, func'), (5, infix')]
    where
      record' = RecordE <$> arbitrary
      literal' = LiteralE <$> arbitrary
      func' = do
          f <- elements functions
          args <- sequence $ do
            t <- fst $ fType f
            pure $ arbitrary `suchThat` (\x -> typeOf x == t)
          pure $ FunctionE f args
      infix' = do
        f <- elements functions
        [a1, a2] <- sequence [arbitrary, arbitrary]
        pure $ InfixE f (litd a1, litd a2)

spec :: Spec
spec = do
  let exprIt e p = it e . p $ parse'' expr e
  describe "specific" $ do
    let exprIt' s e = exprIt s (`shouldBe` Right e)
    describe "simple" $ do
      exprIt' "1" $ litn 1
      exprIt' "test" $ RecordE ()
      exprIt' "tf 1 2" $ FunctionE tf [litn 1, litn 2]
      exprIt' "1 + 2" $ InfixE plus (litn 1, litn 2)
      exprIt' "1 - -2" $ InfixE minus (litn 1, litn (-2))
      exprIt' "1 `tf` -2" $ InfixE tf (litn 1, litn (-2))
    describe "nested" $ do
      exprIt' "1 + (2)" $ InfixE plus (litn 1, litn 2)
      exprIt' "1 + 2 + 3" $ InfixE plus (litn 1, InfixE plus (litn 2, litn 3))
      exprIt' "(1 + 2 + 3)" $ InfixE plus (litn 1, InfixE plus (litn 2, litn 3))
      exprIt' "1 * (2) + 3" $ InfixE plus (InfixE mult (litn 1, litn 2), litn 3)
      exprIt' "(1 + 2) + 3" $ InfixE plus (InfixE plus (litn 1, litn 2), litn 3)
      exprIt' "((1 + 2) + 3)" $ InfixE plus (InfixE plus (litn 1, litn 2), litn 3)
      exprIt' "(tf 1 2) + 3" $ InfixE plus (FunctionE tf [litn 1, litn 2], litn 3)
      exprIt' "tf 1 2 + 3" $ InfixE plus (FunctionE tf [litn 1, litn 2], litn 3)
      exprIt' "tf (1 + 2) 3" $ FunctionE tf [InfixE plus (litn 1, litn 2), litn 3]
  describe "fail - bad syntax" $ do
    let failIt = flip exprIt (`shouldSatisfy` isLeft)
    failIt "tf 1 + 2 3"
    failIt "1 --2"
  describe "fail - bad type" $ do
    let failIt = flip exprIt (`shouldSatisfy` isLeft)
    failIt "1 < 3 + \"asdasd\""
    failIt "tf 1"
  prop "arbitrary expr" . withMaxSuccess 5000 $ \e ->
    parse'' expr (printExpr e) `shouldBe` Right e
