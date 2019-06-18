module Fibonacci
    (
    ) where

import Data.List (elemIndex)

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
nats = streamFromSeed (+1) 1

interLeaveStream :: Stream a -> Stream a -> Stream a
interLeaveStream (Cons a xs) (Cons b ys) = Cons a (Cons b (interLeaveStream xs ys))

interLeaveStream' :: Stream a -> Stream a -> Stream a
interLeaveStream' (Cons a xs) ys = Cons a (interLeaveStream' ys xs)

ruler :: Stream Integer
ruler = interLeaveStream xs ys where
   xs = stream [0,0..]
   ys = stream [1..]

ruler' :: Stream Integer
ruler' = startRuler 0

startRuler :: Integer -> Stream Integer
startRuler y = interLeaveStream (streamRepeat y) (startRuler (y + 1))
