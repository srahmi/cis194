{-# OPTIONS_GHC -Wall #-}
module Calculator
    (eval
    ) where

import ExprT
import Parser

eval :: ExprT -> Integer
eval Lit x   = x
eval Add x y = eval x + eval y
eval Mul x y = eval x * eval y


evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

reify :: ExprT -> ExprT
reify = id


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
