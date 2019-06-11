{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module ExprT where

import StackVM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance HasVars VarExprT where
  var name = Var name

instance Expr VarExprT where
  lit a = VLit a
  add a b = VAdd a b
  mul a b = VMul a b

instance Expr ExprT where
  lit a   = ExprT.Lit a
  add a b = ExprT.Add a b
  mul a b = ExprT.Mul a b

instance HasVars (M.Map String Integer -> Maybe Integer)

instance Expr Integer where
  lit a   = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit a   = a > 0
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit a
    | a >= 0 && a <= 6 = Mod7 (lit a)
    | otherwise        = Mod7 0
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

instance Expr Program where
  lit a = [PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age = Age Integer deriving(Eq,Show)
newtype Year = Year Integer deriving(Eq,Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
