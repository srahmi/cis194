module Golf
    (
    ) where

import Data.List (group, sort)
import Control.Monad (join)
import qualified Data.Map as M
import Control.Arrow (second)

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

histogram :: [Integer] -> String
histogram = addAxis . print' . line . join . map (zip [1..]) . group . sort

line :: [(Integer, Integer)] -> [(Integer, [Integer])]
line = reverse
       . map (second sort)
       . M.toList
       . M.fromListWith (++)
       . map(\(x, y) -> (x, [y]))

print'              :: [(Integer, [Integer])] -> String
print' []           = []
print' ((_, xs):ys) = map(\i -> if i `elem` xs then '*' else ' ') [0..9]
                         ++ "\n" ++ print' ys

addAxis :: String -> String
addAxis s = s ++ "==========\n0123456789\n"
