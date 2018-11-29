module Language.SFL
  ( module Language.SFL.Type
  , module Language.SFL.Lexer
  , module Language.SFL.Parser
  , module Language.SFL.Evaluator
  , compileExpr
  ) where

import           Control.Monad.State.Lazy
import           Data.List.NonEmpty
import           Language.SFL.Evaluator
import           Language.SFL.Lexer
import           Language.SFL.Parser
import           Language.SFL.Type
import           Text.Megaparsec

compileExpr :: (Eq a, RecordField a) => SflParseState a -> String -> Either [ParseError String SflParseError] (Program a)
compileExpr init arg =
  let f = flip evalState init . runSflParser $ runParserT (expr <* eof) "" arg
  in case f of
    Right r -> Right $ compile r
    Left l  -> Left . toList $ bundleErrors l
