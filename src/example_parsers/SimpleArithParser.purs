module SimpleArithParser where

import Prelude hiding (zero)
import Data.Ord
import Data.Int
import Data.Maybe
import Data.String
import Data.Functor
import Control.Alt
import TinyParsec
import Data.Eulalie.Char.Predicates

data ArithExpr 
    = PlusOp ArithExpr ArithExpr
    | MinusOp ArithExpr ArithExpr
    | MulOp ArithExpr ArithExpr
    | LitInt Int


intParserForArithExpr :: Parser ArithExpr
intParserForArithExpr = map LitInt intParser 

exprInsideBrackets :: Parser ArithExpr
exprInsideBrackets = do
    _ <- symb "("
    out <- expr
    _ <- symb ")"
    pure out

factor :: Parser ArithExpr
factor = do
    _ <- pure 1
    intParserForArithExpr <|> exprInsideBrackets

term :: Parser ArithExpr
term = do
    _ <- pure 1
    factor `chainl1` mulOp

mulOp = do
    _ <- symb "*"
    pure MulOp

expr :: Parser ArithExpr
expr = do
    _ <- pure 1
    term `chainl1` plusOp

plusOp = (do
         _ <- symb "+"
         pure PlusOp) <|>
         (do
         _ <- symb "-"
         pure MinusOp)

instance showArithExpr :: Show ArithExpr where
    show (PlusOp x y) = "PlusOp " <> show x <> " " <> show y
    show (MinusOp x y) = "MinusOp " <> show x <> " " <> show y
    show (MulOp x y) = "MulOp " <> show x <> " " <> show y
    show (LitInt x) = show x

eval :: ArithExpr -> Int
eval (PlusOp x y) = eval x + eval y
eval (MinusOp x y) = eval x - eval y
eval (MulOp x y) = eval x * eval y
eval (LitInt x) = x
