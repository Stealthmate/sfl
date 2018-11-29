module Language.SFL.Util where

import           Language.SFL.Const
import           Language.SFL.Type

isOperator :: Function -> Bool
isOperator (Function n ft _ _) = any (`elem` n) operatorAlphabet
