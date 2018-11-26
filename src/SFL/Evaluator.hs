module SFL.Evaluator where

import           SFL.Type

eval :: RecordField a => RecordOf a -> Expr a -> Value
eval record (RecordE r) = recordValue record r
eval record (LiteralE l) =
  case l of
    NumberL n -> NumberV n
    StringL s -> StringV s
eval record (FunctionE f args) = fOp f $ eval record <$> args
eval record (InfixE f (a1, a2)) = fOp f [eval record a1, eval record a2]

compile :: RecordField a => Expr a -> Program a
compile (RecordE r) = flip recordValue r
compile (LiteralE l) = \_ ->
  case l of
    NumberL n -> NumberV n
    StringL s -> StringV s
compile (FunctionE f args) = \r -> fOp f [compile x r | x <- args]
compile (InfixE f (a1, a2)) = \r -> fOp f [compile a1 r, compile a2 r]
