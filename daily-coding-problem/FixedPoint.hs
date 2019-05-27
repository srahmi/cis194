module FixedPoint
    (
    ) where


fixedPoint :: [Int] -> Either Bool Int
fixedPoint [] = Left False
