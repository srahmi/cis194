module FixedPoint
    (fixedPoint) where

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
