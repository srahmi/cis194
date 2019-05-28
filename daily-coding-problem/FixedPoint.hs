module FixedPoint
    (fixedPoint) where

{--
This problem was asked by Apple.

A fixed point in an array is an element whose value is equal to its index.
Given a sorted array of distinct elements, return a fixed point, if one exists.
Otherwise, return False.

For example, given [-6, 0, 2, 40]
you should return 2. Given [1, 5, 7, 8], you should return False.
--}
fixedPoint :: [Integer] -> Either Bool Integer
fixedPoint xs = case fp xs of
  Just x -> Right x
  _      -> Left False

fp :: [Integer] -> Maybe Integer
fp =  head'
         . filter (uncurry (==))
         . zip [0..]

head' :: [(Integer, Integer)] -> Maybe Integer
head' []    = Nothing
head' (x:_) = Just (fst x)
