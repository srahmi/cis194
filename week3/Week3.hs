data IntList = Empty | Cons Int IntList
  deriving Show

absAll             :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll             :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

exampelList = Cons (-1) (Cons 2 (Cons (-6) Empty))
addOne x = x + 1
square x = x * x

mapIntList               :: (Int -> Int) -> IntList -> IntList
mapIntList f Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

keepOnlyEven             :: IntList -> IntList
keepOnlyEven Empty       = Empty
keepOnlyEven (Cons x xs)
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

data List t = E | C t (List t)
  deriving Show

lst1 :: List Int
lst1 = C 1 (C 2 (C 5 E))

lst2 :: List Char
lst2 = C 'A' (C 'B' (C 'C' E))

lst3 :: List Bool
lst3 = C True (C False (C True E))

filterList     :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x    = C x (filterList p xs)
  | otherwise = filterList p xs

mapList           :: (a -> b) -> List a -> List b
mapList _ E       = E
mapLis f (C x xs) = C (f x) (mapList f xs)

safeHead       :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
