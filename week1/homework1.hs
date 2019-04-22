toDigits :: Integer -> [Integer]
toDigits cardNumber
   | cardNumber <= 0  = []
   | otherwise        = toDigits (cardNumber `div` 10) ++ [cardNumber `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOhther :: [Integer] -> [Integer]
doubleEveryOhther []             = []
doubleEveryOhther [x]            = [x]
doubleEveryOhther (x : (y : zs)) = x : y * 2 : doubleEveryOhther zs

sumDigits :: [Integer] -> Integer
sumDigits []  = 0
sumDigits [x] = x
sumDigits xs  = sum (concatMap toDigits xs)

validate :: Integer -> Bool
validate =  isValid . sumDigits . doubleEveryOhther . toDigitsRev

isValid :: Integer -> Bool
isValid n
  | n <= 0          = False
  | n `mod` 10 == 0 = True
  | otherwise       = False
