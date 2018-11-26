module SFL.ParserSpec where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Either
import qualified Data.Text                as Text
import           Data.Word
import           SFL.Parser
import           SFL.Type
import           SFL.Util
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Megaparsec

instance Typed () where
  typeOf () = StringType
instance Record () where
  toRecordId () = "test"
  fromRecordId "test" = Just ()
  fromRecordId _      = Nothing

testF = Function "tf" ([NumberType, NumberType], NumberType) (\[NumberV x] -> NumberV x) 7
testF2 = Function "tf2" ([NumberType, NumberType], NumberType) (\[NumberV x, NumberV y] -> NumberV x) 7
plus = Function "+" ([NumberType, NumberType], NumberType) (\[NumberV x, NumberV y] -> NumberV $ x + y) 6
mult = Function "*" ([NumberType, NumberType], NumberType) (\[NumberV x, NumberV y] -> NumberV $ x * y) 7
lessThan = Function "<" ([NumberType, NumberType], BoolType) (\[NumberV x, NumberV y] -> BoolV $ x < y) 5

functions = [testF, testF2, lessThan, plus, mult]

parse'' = flip parse' (SflParseState functions :: SflParseState ())

instance Arbitrary Literal where
  arbitrary = do
    n <- arbitrary :: Gen Word
    case n `rem` 2 of
      0 -> StringL <$> pure "test"
      1 -> NumberL . abs <$> arbitrary

instance (Arbitrary a, Record a) => Arbitrary (Expr a) where
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
        pure $ InfixE f (LiteralE $ NumberL a1, LiteralE $ NumberL a2)

exprIt e p = it e . p $ parse'' expr e

spec :: Spec
spec = do
  describe "specific" $ do
    let exprIt' s p = exprIt s (`shouldBe` p)
    exprIt' "1 + (2)" $ Right (InfixE plus (LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0))
    exprIt' "1 + 2 + 3" $ Right (InfixE plus (LiteralE $ NumberL 1.0, InfixE plus (LiteralE $ NumberL 2.0, LiteralE $ NumberL 3.0)))
    exprIt' "1 * (2) + 3" $ Right (InfixE plus (InfixE mult (LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0), LiteralE $ NumberL 3.0))
    exprIt' "(1 + 2) + 3" $ Right (InfixE plus (InfixE plus (LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0), LiteralE $ NumberL 3.0))
    exprIt' "(1 + 2 + 3)" $ Right (InfixE plus (LiteralE $ NumberL 1.0, InfixE plus (LiteralE $ NumberL 2.0, LiteralE $ NumberL 3.0)))
    exprIt' "((1 + 2) + 3)" $ Right (InfixE plus (InfixE plus (LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0), LiteralE $ NumberL 3.0))
    exprIt' "(tf 1 2) + 3" $ Right (InfixE plus (FunctionE testF [LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0], LiteralE $ NumberL 3.0))
    exprIt' "tf 1 2 + 3" $ Right (InfixE plus (FunctionE testF [LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0], LiteralE $ NumberL 3.0))
    exprIt' "tf (1 + 2) 3" $ Right $ FunctionE testF [InfixE plus (LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0), LiteralE $ NumberL 3.0]
  describe "fail" $ do
    let failIt = flip exprIt (`shouldSatisfy` isLeft)
    failIt "tf 1 + 2 3"
  describe "expr" $ do
    it "parses literal" $ do
      let parseL s l = parse'' expr s `shouldBe` Right (LiteralE l)
      parseL "1.0" (NumberL 1.0)
      parseL "\"asd\"" (StringL "asd")
    it "simple" $ do
      parse'' expr "1 < 3" `shouldBe` Right (InfixE lessThan (LiteralE $ NumberL 1.0, LiteralE $ NumberL 3.0))
      parse'' expr "tf 1 2" `shouldBe` Right (FunctionE testF [LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0])
      parse'' expr "1 `tf2` 2" `shouldBe` Right (InfixE testF2 (LiteralE $ NumberL 1.0, LiteralE $ NumberL 2.0))
    it "does not parse bad typed expr" $
      parse'' expr "1 < 3 + \"asdasd\"" `shouldSatisfy` isLeft
    it "does not parse expr with few arguments" $
      parse'' expr "tf 1" `shouldSatisfy` isLeft
    prop "arbitrary expr" . withMaxSuccess 10000 $ \e -> do
      print $ printExpr' e
      parse'' expr (printExpr' e) `shouldBe` Right e
