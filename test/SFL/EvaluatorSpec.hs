{-# LANGUAGE TemplateHaskell #-}
module SFL.EvaluatorSpec where

import           SFL.Evaluator
import           SFL.Type
import           Test.Hspec
import SFL.TH
import           TestUtil

data MyRecord = MyRecord
  { f1 :: String
  , f2 :: Int
  }

$(deriveRecordField ''MyRecord)

instance RecordField RecordFieldMyRecord where
  type RecordOf RecordFieldMyRecord = MyRecord
  fromRecordId "f1" = Just F1
  fromRecordId "f2" = Just F2
  fromRecordId _    = Nothing
  toRecordId F1 = "f1"
  toRecordId F2 = "f2"
  recordValue r F1 = StringV $ f1 r
  recordValue r F2 = NumberV . fromIntegral $ f2 r

spec :: Spec
spec =
  describe "eval" $ do
    let testRecord = MyRecord "Hello" 1
    let evalIt s e p = it s . p $ eval testRecord e
    let plus = Function "+" ([], NumberType) (\[NumberV a1, NumberV a2] -> NumberV $ a1 + a2) 0
    evalIt "f1" (RecordE F1) (`shouldBe` StringV "Hello")
    evalIt "34" (litn 34 :: Expr RecordFieldMyRecord) (`shouldBe` NumberV 34.0)
    evalIt "[+] 34 f2" (FunctionE plus [litn 34, RecordE F2]) (`shouldBe` NumberV 35.0)
