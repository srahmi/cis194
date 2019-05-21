module Golf
    (
    ) where

import Data.List (group, sort)
import Control.Monad (join)
import qualified Data.Map as M
import Control.Arrow (second)
import Data.Monoid

{--
From 0 to xs length return an array of every n-th element in xs
and filter empty array
 --}
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
{--
Lets take this array as example [5,1,1,1]
            " *        \n *        \n *   *    \n==========\n0123456789\n"
            appendAxis
             " *        \n *        \n *   *    \n"
             . concatMap print'
             [(3,[1]),(2,[1]),(1,[1,5])]
             . lines'
             [(1,1),(1,2),(1,3),(5,1)]
             . join
             [[(1,1),(1,2),(1,3)],[(5,1)]]
             . map (zip [1..])
             [[1,1,1],[5]]
             . group
             [1,1,1,5]
             . sort
--}
histogram :: [Integer] -> String
histogram =  appendAxis
             . concatMap print' . lines'
             . join
             . map (zip [1..])
             . group . sort

lines' :: [(Integer, Integer)] -> [(Integer, [Integer])]
lines' = reverse
       . map (second sort)
       . M.toList
       . M.fromListWith (++)
       . map(\(x, y) -> (x, [y]))

print'              :: (Integer, [Integer]) -> String
print' (_, xs) = map(\i -> if i `elem` xs then '*' else ' ') [0..9] ++ "\n"

appendAxis :: String -> String
appendAxis s = s ++ "==========\n0123456789\n"
