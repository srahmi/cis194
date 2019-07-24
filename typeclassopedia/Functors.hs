module Functors where

    data Pair a = Pair a a

    data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

    instance Functor ((,) e) where
        fmap g (e, a) = (e, g a)

    instance Functor Pair where
        fmap g (Pair a b)  = Pair (g a) (g b)

    instance Functor ITree where
        fmap g (Leaf h)  = Leaf (g . h)
        fmap g (Node xs) = Node (fmap g xs)
        