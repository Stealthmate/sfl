{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Language.SFL.LexerSpec where

import           Data.Either           (isLeft)
import qualified Data.Set              as Set
import           Language.SFL.Const
import           Language.SFL.Lexer
import           Language.SFL.Type
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Test.QuickCheck       as QC
import           TestUtil

quote :: String -> String
quote s = "\"" <> s <> "\""

escape = filter (/= '\\')

data TestRecord = A | B deriving (Enum, Bounded, Show, Eq)
instance Typed TestRecord where
  typeOf _ = StringType
instance RecordField TestRecord where
  type RecordOf TestRecord = TestRecord
  fromRecordId "a" = Just A
  fromRecordId "b" = Just B
  fromRecordId _   = Nothing
  toRecordId A = "a"
  toRecordId B = "b"
  recordValue r = StringV . show

newtype OperatorString = OperatorString String deriving (Eq, Show)
instance QC.Arbitrary OperatorString where
  arbitrary = do
    s <- QC.sublistOf (Set.toList operatorAlphabet) `QC.suchThat` (not . null)
    s' <- QC.shuffle s
    pure $ OperatorString s'

parse'' = flip parse' (SflParseState [] :: SflParseState TestRecord)
parseIt s pred p = it s . pred $ parse'' p s

spec :: Spec
spec = do
  describe "recordId" $ do
    let ridIt s p = parseIt s p recordId
    describe "id" $ do
      let ridIt' r = ridIt (toRecordId r) (`shouldBe` Right r)
      ridIt' A
      ridIt' B
    describe "fail" $ do
      let failIt = flip ridIt (`shouldSatisfy` isLeft)
      failIt "aasdasd"
      failIt "asd "
      failIt "`1asdasd`"

  describe "literal" $ do
    let litIt s p = parseIt s p literal
    describe "string" $ do
      let litItS s = litIt (quote s) (`shouldBe` Right (StringL $ escape s))
      litItS "asd"
      litItS "\\\"asdasd\\\""
      litItS "a\ns"
    describe "number" $ do
      prop "parses an int" $ \(n :: Int) ->
        parse'' literal (show n) `shouldBe` Right (NumberL (fromIntegral n))
      prop "parses parses a float" $ \f ->
        parse'' literal (show f) `shouldBe` Right (NumberL f)
    describe "fail" $ do
      let failIt x = litIt x (`shouldSatisfy` isLeft)
      failIt "123asd"
      failIt "asd123"
      failIt "asd\"sdasd"
      failIt "asd\"sdasd"

  describe "identifier" $ do
    let idIt s p = parseIt s p identifier
    describe "function name" $ do
      let idItF s = idIt s (`shouldBe` Right (filter (/= ' ') s))
      idItF "test"
      idItF "_test"
      idItF "testF"
      idItF "testF2"
    describe "fail" $ do
      let failIt = flip idIt (`shouldSatisfy` isLeft)
      failIt "\\asd"
      failIt "12asd"

  describe "infixFunction" $
    prop "operator" $ \(OperatorString s) ->
      parse'' infixFunction s `shouldBe` Right (OperatorInf s)
