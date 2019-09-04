module Functors where

    data Pair a = Pair a a

    data ITree a = Leaf (Int -> a)
             | Node [ITree a]

    data Tree' a = Node' a [Tree' a]

    data Either' a b = Left' a | Right' b
      deriving (Show)

    data Maybe' a = Just' a | Nothing'
      deriving (Show)

    newtype ZipList a = ZipList { getZipList :: [a] }

    -- instance Functor ((,) e) where
    --     fmap g (e, a) = (e, g a)

    instance Functor Pair where
        fmap g (Pair a b)  = Pair (g a) (g b)

    instance Functor ITree where
        fmap f (Leaf h)  = Leaf (f . h)
        fmap f (Node xs) = Node (map (fmap f) xs)

    -- instance Functor ((->) e) where                       --    g           f         g . f
        -- fmap :: (a -> b) -> (->) e a -> (->) e b === fmap :: (a -> b) -> (e -> a) -> (e -> b) === (.)
        -- fmap g (e -> a) = (.)

    instance Functor Tree' where
        fmap = fmapTree
          where fmapTree f (Node' x ts) = Node' (f x) (map (fmapTree f) ts)

    instance Functor (Either' a) where
        fmap _ (Left' a) = Left' a
        fmap f (Right' b) = Right' (f b)

    instance Functor Maybe' where
        fmap f (Just' x) = Just' (f x)
        fmap _ Nothing'  = Nothing'

    instance Functor ZipList where
        fmap f (ZipList xs) = ZipList (map f xs)
