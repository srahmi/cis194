toDigits :: Integer -> [Integer]
toDigits cardNumber
   | cardNumber <= 0  = []
   | otherwise        = toDigits (cardNumber `div` 10) ++ [cardNumber `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOhther :: [Integer] -> [Integer]
doubleEveryOhther [] = []
doubleEveryOhther (x : []) = [x]
doubleEveryOhther (x : (y : zs)) = x : y * 2 : doubleEveryOhther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = []
sumDigits [x] = [x]
sumDigits xs = map toDigits xs
