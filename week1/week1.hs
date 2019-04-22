sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
   | isEven n        = n `div` 2
   | otherwise       = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0              = 0
  | n `mod` 17 == 2    = -43
  | otherwise          = n + 3

isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  | otherwise      = False

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

hello1 :: [Char]
hello1 = ['H','e','l','o']

hello2 :: String
hello2 = "Helo"

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []        = []
sumEveryTwo (x:[])    = [x]
sumEveryTwo (x:(y:z)) = x + y : sumEveryTwo z

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
