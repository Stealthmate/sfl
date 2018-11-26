{-# LANGUAGE TypeFamilies #-}
module SFL.EvaluatorSpec where

import           SFL.Evaluator
import           SFL.Type
import           Test.Hspec

data MyRecord = MyRecord
  { f1 :: String
  , f2 :: Int
  }

data MyRecordSpec =
    F1
  | F2
  deriving (Enum, Bounded)
instance Typed MyRecordSpec where
  typeOf F1 = StringType
  typeOf F2 = NumberType
instance RecordField MyRecordSpec where
  type RecordOf MyRecordSpec = MyRecord
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
    evalIt "34" (LiteralE $ NumberL 34.0 :: Expr MyRecordSpec) (`shouldBe` NumberV 34.0)
    evalIt "[+] 34 f2" (FunctionE plus [LiteralE $ NumberL 34.0, RecordE F2]) (`shouldBe` NumberV 35.0)
