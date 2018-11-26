module SFL.Util where

import           SFL.Const
import           SFL.Type

isOperator :: Function -> Bool
isOperator (Function n ft _ _) = any (`elem` n) operatorAlphabet
