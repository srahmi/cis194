module Week12 where

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

addOneOrTwo' :: Int -> [(Int, Int)]
addOneOrTwo' x = if x > 20 then [(x + 1, x + 2)] else [(x - 1, x - 2)]

