module Wholemeal
    (
    ) where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
 | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum
        . filter even
        . takeWhile (/= 1)
        . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert        :: a -> Tree a -> Tree a
insert e Leaf = Node 0 Leaf e Leaf
insert e (Node _ l v r)
      | height l > height r =  Node (height l + 1) l v (insert e r)
      | otherwise           =  Node (height r + 1) (insert e l) v r

height      :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h
