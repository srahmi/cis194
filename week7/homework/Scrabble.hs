{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Semigroup
import Data.List(find)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)


instance Semigroup Score where
  Score a <> Score b = Score (a + b)

instance Monoid Score where
  mempty = Score 0
  Score a `mappend` Score b = Score (a + b)

scoreTable :: [(Char, Score)]
scoreTable =
 [('a', Score 1), ('b', Score 3),('c', Score 3),
  ('d', Score 2),('e', Score 1),('f', Score 4),
  ('g', Score 2),('h', Score 4),('i', Score 1),
  ('j', Score 8),('k', Score 5),('l', Score 1),
  ('m', Score 3),('n', Score 1),('o', Score 1),
  ('p', Score 3),('q', Score 10),('r', Score 1),
  ('s', Score 1),('t', Score 1),('u', Score 1),
  ('v', Score 4),('w', Score 4),('x', Score 8),
  ('y', Score 4),('z', Score 10)]

score :: Char -> Score
score c
  | c `elem` ['a'..'z'] = toScore $ find (\(c', _) -> c == c') scoreTable
  | otherwise = Score 0

toScore :: Maybe (Char, Score) -> Score
toScore a = case a of
  Just x -> snd x
  _      -> Score 0

scoreString :: String -> Score
scoreString =  mconcat . map score
