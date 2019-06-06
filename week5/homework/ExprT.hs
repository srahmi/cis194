module ExprT where

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr ExprT where
  lit a = Lit a
  add a b = Add a b
  mul a b = Mul a b 
