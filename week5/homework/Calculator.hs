{-# OPTIONS_GHC -Wall #-}
module Calculator
    (eval
    ) where

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit x)   = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

compile :: String -> Maybe Program
compile = parseExp lit add mul
