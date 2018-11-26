{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
module SFL.Type where

import           Control.Monad.State.Lazy as ST
import           Data.Data                hiding (typeOf)
import           Data.Typeable            hiding (typeOf)
import           Text.Megaparsec

newtype SflParseState a = SflParseState
  { sflpFunctions   :: [Function]
  }

class Typed a where
  typeOf :: a -> ValueType
class (Enum a, Bounded a, Typed a) => Record a where
  type RecordOf a
  fromRecordId :: String -> Maybe a
  toRecordId :: a -> String
  recordValue :: RecordOf a -> a -> Value
class PrintExpr e where
  printExpr :: e -> String

newtype SflParserM b a = SflM { runSflParser :: ST.State (SflParseState b) a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadState (SflParseState b))

data CustomParseError =
    UnknownFunction String
  | WrongType ValueType ValueType
  | BadNumberOfArguments String Int Int
  | NakedInfixInsideFunction
  | EndResultIsNotBool
  deriving (Eq, Data, Typeable, Ord, Read, Show)
instance ShowErrorComponent CustomParseError where
  showErrorComponent NakedInfixInsideFunction = "Naked infix inside function"
  showErrorComponent (UnknownFunction x)  = "Unknown function: " ++ x
  showErrorComponent (WrongType expected actual)        = "Expected expr of type " ++ show expected ++ " but got " ++ show actual
  showErrorComponent (BadNumberOfArguments f expected actual) = "Expected " ++ show expected ++ " arguments to function " ++ f ++ " but found " ++ show actual
type SFLP a = ParsecT CustomParseError String (SflParserM a)

data Literal =
    NumberL Double
  | StringL String deriving (Eq)
instance Show Literal where
  show (NumberL n) = show n
  show (StringL s) = show s
instance Typed Literal where
  typeOf (NumberL _) = NumberType
  typeOf (StringL _) = StringType

data Symbol =
    LeftParenS
  | RightParenS deriving (Eq)
instance Show Symbol where
  show LeftParenS  = "("
  show RightParenS = ")"

data Infix =
    FunctionInf String
  | OperatorInf String
  deriving (Eq, Show)

----------------------- AST

data ValueType =
    NumberType
  | StringType
  | BoolType
  deriving (Eq, Show, Read, Enum, Ord, Data)

data Value =
    NumberV Double
  | StringV String
  | BoolV Bool
  deriving (Eq, Show)

data Function = Function
  { fName       :: String
  , fType       :: ([ValueType], ValueType)
  , fOp         :: [Value] -> Value
  , fPrecedence :: Int
  }
instance Show Function where
  show (Function name _ _ _) = show name
instance Eq Function where
  (Function name t _ _) == (Function name' t' _ _) = (name == name') && (t == t')
instance Typed Function where
  typeOf (Function _ ft _ _) = snd ft

data Expr a =
    RecordE a
  | LiteralE Literal
  | FunctionE Function [Expr a]
  | InfixE Function (Expr a, Expr a)
  deriving Eq
instance (Record a) => Show (Expr a) where
  show (RecordE a)  = toRecordId a
  show (LiteralE l) = show l
  show e            = printExpr' e
instance (Record a) => Typed (Expr a) where
  typeOf (RecordE a)     = typeOf a
  typeOf (LiteralE l)    = typeOf l
  typeOf (FunctionE o _) = typeOf o
  typeOf (InfixE o _)    = typeOf o

instance PrintExpr Literal where
  printExpr (StringL s) = "\"" <> concat (escape <$> s) <> "\""
    where
      escape '\n' = "\\n"
      escape '\\' = "\\\\"
      escape x    = [x]
  printExpr (NumberL n) = show n
instance PrintExpr Function where
  printExpr (Function name _ _ _) = name


type Program a = RecordOf a -> Value

operatorAlphabet = "+-*/.<>="

-- parse :: String -> Expr
-- compile :: Expr -> Program

isOperator :: Function -> Bool
isOperator (Function n ft _ _) = any (`elem` n) operatorAlphabet

printInfixFunction :: Function -> String
printInfixFunction f
  | isOperator f = fName f
  | otherwise = "`" <> fName f <> "`"

printPrefixFunction :: Function -> String
printPrefixFunction f
  | isOperator f = "[" <> fName f <> "]"
  | otherwise = fName f

printExpr' :: (Record a) => Expr a -> String
printExpr' (RecordE r) = toRecordId r
printExpr' (LiteralE l) = printExpr l
printExpr' (FunctionE op args) = printPrefixFunction op <> " " <> unwords ((\x -> "(" <> printExpr' x <> ")") <$> args)
printExpr' (InfixE op (x1, x2)) = "(" <> printExpr' x1 <> ") " <> printInfixFunction op <> " (" <> printExpr' x2 <> ")"

