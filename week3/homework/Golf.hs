module Golf
    (
    ) where

skips    :: [a] -> [[a]]
skips [] = []
skips xs = filter (not . null) $ map (`every` xs) index
  where
    index = [0, 1..(length xs)]

every      :: Int -> [a] -> [a]
every n xs = case drop n xs of
  (y:ys) -> y : every n ys
  []     -> []

localMaxima    :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs = case xs of
  (x:y:z:ys) -> if y > x && y > z
    then y : localMaxima (y:z:ys)
    else localMaxima (y:z:ys)
  otherwise -> []

localMaxima'            :: [Integer] -> [Integer]
localMaxima' []         = []
localMaxima' [_]        = []
localMaxima' [_, _]     = []
localMaxima' (x:y:z:ys)
     | y > x && y > z = y : localMaxima' (y:z:ys)
     | otherwise      = localMaxima' (y:z:ys)
