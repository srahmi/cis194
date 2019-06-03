module Week5
    (
    ) where

f1   :: a -> a
f1 x = x

f2 :: a -> b
f2 x = undefined

f3     :: a -> b -> a
f3 x y = x

f4    :: [a] -> [a]
f4 xs = xs

f5 :: (b -> c) -> (a -> b) -> (a -> c)
f5 g h = g . h

f6     :: (a -> a) -> a -> a
f6 f x = x
