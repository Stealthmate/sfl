{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module SFL.Parser where

import           Control.Monad.State.Lazy
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           SFL.Lexer
import           SFL.Type                   as SFLPT
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)


checkName :: String -> SFLP b Function
checkName x = do
  xs <- sflpFunctions <$> get
  case filter (\y -> fName y == x) xs of
    []  -> customFailure $ UnknownFunction x
    y:_ -> pure y

nArgs :: Function -> Int
nArgs = length . fst . fType

typeCheck :: Record a => Expr a -> SFLP a ()
typeCheck (FunctionE f args) =
  sequence_ $ do
    (a,e) <- zip args (fst (fType f))
    pure . when (typeOf a /= e) $ customFailure (WrongType e (typeOf a))
typeCheck (InfixE f (x1, x2)) =
  case fst (fType f) of
    [t1, t2] -> sequence_ $ do
      (e, a) <- zip [t1, t2] [typeOf x1, typeOf x2]
      pure . when (e /= a) $ customFailure (WrongType e a)
    _ -> customFailure $ BadNumberOfArguments (fName f) 2 (nArgs f)



expr' :: (Eq a, Record a) => SFLP a (Expr a, Bool)
expr' = do
  mlp <- optional leftParen
  e1 <- case mlp of
    Just _ -> do
      e <- expr
      rightParen
      pure e
    Nothing -> nonInfix
  op <- optional infixFunction
  case op of
    Just op' -> (,False) <$> do
      f <- case op' of
        FunctionInf name -> checkName name
        OperatorInf name -> checkName name
      (e2, hasParens) <- expr'
      let theExpr =
            if not hasParens
              then
                case e2 of
                  InfixE f2 (x1, x2)
                    | fPrecedence f > fPrecedence f2 -> InfixE f2 (InfixE f (e1, x1), x2)
                    | otherwise -> InfixE f (e1, e2)
                  _ -> InfixE f (e1, e2)
              else InfixE f (e1, e2)
      typeCheck theExpr
      pure theExpr
    Nothing -> pure (e1, isJust mlp)
  where
    nonInfix = choice [ RecordE <$> try recordId, LiteralE <$> literal, pFunction ]
    pFunction = do
      name <- identifier
      f <- checkName name
      args <- sequence $ do
        i <- [1..(nArgs f - 1)]
        pure $ do
          (e, hasParens) <- expr'
          case e of
            InfixE _ _
              | not hasParens -> customFailure NakedInfixInsideFunction
              | otherwise -> pure e
            _ -> pure e
      exp <- do
        (e, hasParens) <- expr'
        pure $ case e of
          InfixE f2 (x1, x2)
            | not hasParens -> InfixE f2 (FunctionE f (args ++ [x1]), x2)
            | otherwise -> FunctionE f (args ++ [e])
          _ -> FunctionE f (args ++ [e])
      typeCheck exp
      pure exp

expr :: (Eq a, Record a) => SFLP a (Expr a)
expr = fst <$> expr'
