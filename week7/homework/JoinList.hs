{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized
import Scrabble

{--
Append (Product 210)
  (Append (Product 30)
    (Single (Product 5) ’y’)
    (Append (Product 6)
        (Single (Product 2) ’e’)
        (Single (Product 3) ’a’)))
  (Single (Product 7) ’h’)
--}
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Append (Size 4)
--  (Single (Size 1) 'y')
--  (Append (Size 3)
--    (Single (Size 1) 'e')
--    (Append (Size 2)
--      (Single (Size 1) 'a')
--      (Single (Size 1) 'h')))
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append m jl1 jl2)
  | i < 0 || i > getSize (size m) = Nothing
  | i < size' jl1 = indexJ i jl1
  | otherwise = indexJ (i-1) jl2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl    = jl
dropJ _ Empty = Empty
dropJ i jl@(Single m _)
  | i > getSize (size m) = jl
  | otherwise            = Empty
dropJ i jl@(Append m jl1 jl2)
  | i > getSize (size m) = Empty
  | i < 0                = jl
  | i < size' jl1        =  (+++) jl1 jl2
  | otherwise            = dropJ (i-1) jl2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _     = Empty
takeJ _ Empty = Empty
takeJ i jl@(Single m _)
  | i > getSize (size m) = jl
  | otherwise            = Empty
takeJ i jl@(Append m jl1 jl2)
  | i >= getSize (size m) = jl
  | i <= 0               = Empty
  | otherwise            = (+++) jl1 $ takeJ (i-1) jl2

size' :: (Sized m, Monoid m) => JoinList m a -> Int
size' jl = getSize $ size (tag jl)

scoreLine :: String -> JoinList Score String
scoreLine line = Single (scoreString line) line

az :: JoinList Size Char
az = foldr1 (+++) $ Single (Size 1) <$> "yeah"
