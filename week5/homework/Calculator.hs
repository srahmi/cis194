{-# OPTIONS_GHC -Wall #-}
module Calculator
    (eval
    ) where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x)               = x
eval (Add (Lit x) (Lit y)) = x + y
eval (Mul (Lit x) (Lit y)) = x * y
eval exprT = case exprT of
  (Add (Lit x) expr)  -> x + eval expr
  (Add expr (Lit y))  -> eval expr + y
  (Mul (Lit x) expr)  -> x * eval expr
  (Mul expr (Lit y))  -> eval expr * y
  _                   -> 0

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

reify :: ExprT -> ExprT
reify = id
