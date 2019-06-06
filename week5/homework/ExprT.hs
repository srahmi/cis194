module ExprT where

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

newtype MinMax = MinMax Integer deriving (Ord, Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr ExprT where
  lit a   = Lit a
  add a b = Add a b
  mul a b = Mul a b

instance Expr Integer where
  lit a   = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit a   = a > 0
  add a b = a || b
  mul a b = a && b

instance Expr MinMax where
  lit a = MinMax (lit a)
  add   = max
  mul   = min
