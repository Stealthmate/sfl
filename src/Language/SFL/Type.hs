{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
module Language.SFL.Type where

import qualified Control.Monad.State.Lazy as ST
import           Data.Data                hiding (typeOf)
import           Data.Typeable            ()
import           Text.Megaparsec

-- Megaparsec
newtype SflParseState a = SflParseState
  { sflpFunctions   :: [Function]
  }
newtype SflParserM b a = SflM { runSflParser :: ST.State (SflParseState b) a }
  deriving (
      Functor
    , Applicative
    , Monad
    , ST.MonadState (SflParseState b))

data SflParseError =
    UnknownFunction String
  | WrongType ValueType ValueType
  | BadNumberOfArguments String Int Int
  | NakedInfixInsideFunction
  deriving (Eq, Data, Typeable, Ord, Read, Show)
instance ShowErrorComponent SflParseError where
  showErrorComponent NakedInfixInsideFunction = "Naked infix inside function"
  showErrorComponent (UnknownFunction x)  = "Unknown function: " ++ x
  showErrorComponent (WrongType expected actual)        = "Expected expr of type " ++ show expected ++ " but got " ++ show actual
  showErrorComponent (BadNumberOfArguments f expected actual) = "Expected " ++ show expected ++ " arguments to function " ++ f ++ " but found " ++ show actual



type SFLP a = ParsecT SflParseError String (SflParserM a)
-- END

-- Type classes
class Typed a where
  typeOf :: a -> ValueType

class (Enum a, Bounded a, Typed a) => RecordField a where
  type RecordOf a
  fromRecordId :: String -> Maybe a
  toRecordId :: a -> String
  recordValue :: RecordOf a -> a -> Value

-- END

-- Lexer
data Infix =
    FunctionInf String
  | OperatorInf String
  deriving (Eq, Show)
-- END

-- AST
data ValueType =
    NumberType
  | StringType
  | BoolType
  deriving (Eq, Show, Read, Enum, Ord, Data)
data Value =
    NumberV Double
  | StringV String
  | BoolV Bool
  deriving (Eq, Show, Read)

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


data Literal =
    NumberL Double
  | StringL String
  deriving (Eq, Show, Read)
instance Typed Literal where
  typeOf (NumberL _) = NumberType
  typeOf (StringL _) = StringType


data Expr a =
    RecordE a
  | LiteralE Literal
  | FunctionE Function [Expr a]
  | InfixE Function (Expr a, Expr a)
  deriving (Eq, Show)
instance (RecordField a) => Typed (Expr a) where
  typeOf (RecordE a)     = typeOf a
  typeOf (LiteralE l)    = typeOf l
  typeOf (FunctionE o _) = typeOf o
  typeOf (InfixE o _)    = typeOf o


type Program a = RecordOf a -> Value
