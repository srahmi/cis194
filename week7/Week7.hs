{-# OPTIONS_GHC -Wall #-}
module Week7
    (
    ) where

import Data.Monoid()
import Data.Semigroup

sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

elem'    :: (Eq a) => a -> [a] -> Bool
elem' e  = foldl (\acc x -> (x == e) || acc) False

map'    :: (a -> b) -> [a] -> [b]
map' f  = foldr (\x acc -> f x : acc) []

max' :: (Ord a) => [a] -> a
max' = foldl1 (\acc x -> if acc > x then acc else x)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

newtype Sum' a = Sum' a
    deriving (Show)

instance Num a => Semigroup (Sum' a) where
  Sum' x <> Sum' y = Sum' (x + y)

instance Num a => Monoid (Sum' a) where
  mempty                      = Sum' 0
  (Sum' x) `mappend` (Sum' y) = Sum' (x + y)

data Tree a = Empty
            | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

newtype Any' = Any' Bool
     deriving (Show)

newtype All' = All' Bool
     deriving (Show)

instance Semigroup Any' where
  Any' a <> Any' b = Any' (a || b)

instance Semigroup All' where
  All' a <> All' b = All' (a && b)

instance Monoid Any' where
  mempty = Any' False

instance Monoid All' where
  mempty = All' True


leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
