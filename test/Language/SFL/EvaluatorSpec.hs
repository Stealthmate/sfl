{-# LANGUAGE TemplateHaskell #-}
module Language.SFL.EvaluatorSpec where

import           Language.SFL.Evaluator
import           Language.SFL.TH
import           Language.SFL.Type
import           Test.Hspec
import           TestUtil

data MyRecord = MyRecord
  { f1 :: String
  , f2 :: Int
  }
$(deriveRecordField ''MyRecord)

spec :: Spec
spec =
  describe "eval" $ do
    let testRecord = MyRecord "Hello" 1
    let evalIt s e p = it s . p $ eval testRecord e
    let plus = Function "+" ([], NumberType) (\[NumberV a1, NumberV a2] -> NumberV $ a1 + a2) 0
    evalIt "f1" (RecordE F1) (`shouldBe` StringV "Hello")
    evalIt "34" (litn 34 :: Expr RecordFieldMyRecord) (`shouldBe` NumberV 34.0)
    evalIt "[+] 34 f2" (FunctionE plus [litn 34, RecordE F2]) (`shouldBe` NumberV 35.0)