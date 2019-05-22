multThree       :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

devideByTen :: (Floating a) => a -> a
devideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice     :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith'                 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) =  f x y : zipWith' f xs ys

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 = filter (>100)

foo       :: (b -> c) -> (a ->  b) -> a -> c
foo f g x = f (g x)

myTest    :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

schonfinkel       :: ((a, b) -> c) -> a -> b -> c
schonfinkel f x y = f (x, y)

unschonfinkel          :: (a -> b -> c) -> (a, b) -> c
unschonfinkel f (x, y) = f x y

foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map(\x -> 7 * x + 2) . filter (>3)
