module Language.SFL.Printer where

import           Language.SFL.Type
import           Language.SFL.Util

class PrintExpr e where
  printExpr :: e -> String

instance PrintExpr Literal where
  printExpr (StringL s) = "\"" <> concat (escape <$> s) <> "\""
    where
      escape '\n' = "\\n"
      escape '\\' = "\\\\"
      escape x    = [x]
  printExpr (NumberL n) = show n

instance PrintExpr Function where
  printExpr (Function name _ _ _) = name

instance (RecordField a) => PrintExpr (Expr a) where
  printExpr (RecordE r) = toRecordId r
  printExpr (LiteralE l) = printExpr l
  printExpr (FunctionE op args) = printPrefixFunction op <> " " <> unwords ((\x -> "(" <> printExpr x <> ")") <$> args)
    where
      printPrefixFunction f
        | isOperator f = "[" <> fName f <> "]"
        | otherwise = fName f
  printExpr (InfixE op (x1, x2)) = "(" <> printExpr x1 <> ") " <> printInfixFunction op <> " (" <> printExpr x2 <> ")"
    where
      printInfixFunction f
        | isOperator f = fName f
        | otherwise = "`" <> fName f <> "`"

