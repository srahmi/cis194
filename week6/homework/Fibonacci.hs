module Fibonacci
    (
    ) where

data Stream t = Cons t (Stream t)

instance Show a => Show (Stream a)
   where
     show = show . take 20 . streamToList

fib   :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : scanl (+) 1 fibs2

fibs2' :: [Integer]
fibs2' = 0 : 1 : next fibs2'
  where
    next (a : t@(b : _)) = (a + b) : next t

streamToList   :: Stream a -> [a]
streamToList (Cons s xs) = s : streamToList xs

stream :: [a] -> Stream a
stream (x:xs) = Cons x (stream xs)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a xs) = Cons (f a) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = streamMap f (streamRepeat s)

nats :: Stream Integer
nats = stream [0..]

ruler :: Stream Integer
ruler = stream [0..][]
