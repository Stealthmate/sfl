
module Language.SFL.Lexer where

import           Control.Monad.Combinators
import           Language.SFL.Const
import           Language.SFL.Type          as SFLT
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

sc :: SFLP a ()
sc = L.space space1 empty empty

lexeme :: SFLP a b -> SFLP a b
lexeme = L.lexeme sc

escapeChar = '\\'
stringDelimiter = '\"'

literal :: SFLP a SFLT.Literal
literal = lexeme $ choice
  [ StringL <$> try parseString
  , NumberL <$> parseNumber
  ]
  where
    parseString = do
      char stringDelimiter
      s <- manyTill (try escapedSymbol <|> anySingle) (single stringDelimiter)
      pure s :: SFLP a String
    parseNumber = L.signed (pure ()) $ choice [try L.float, fromIntegral <$> L.decimal ] :: SFLP a Double
    escapedSymbol = do
      char escapeChar
      oneOf "\"\\"


recordId :: RecordField a => SFLP a a
recordId =
  let x:xs = (\x -> (toRecordId x, x)) <$> enumFromTo minBound maxBound
      cs = parseRid x : (try . parseRid <$> xs)
  in lexeme . choice $ reverse cs
  where
    parseRid (a,b) = string a >> pure b

identifier :: SFLP a String
identifier = lexeme $ choice [try operator, normal]
  where
    operator = do
      char '['
      i <- some (oneOf operatorAlphabet)
      char ']'
      pure i
    normal = do
      let identifierLetter = try letterChar <|> oneOf "_"
      x <- identifierLetter
      xs <- many (try digitChar <|> identifierLetter)
      pure (x:xs)

infixFunction :: SFLP a Infix
infixFunction = lexeme $ try infixFunction' <|> operator
  where
    infixFunction' = do
      char '`'
      x <- identifier
      char '`'
      pure $ FunctionInf x
    operator = OperatorInf <$> some (oneOf operatorAlphabet)

leftParen :: SFLP a Char
leftParen = lexeme $ char '('

rightParen :: SFLP a Char
rightParen = lexeme $ char ')'
