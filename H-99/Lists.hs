module Lists
    (
    ) where

      data NestedList a = Elem a | List [NestedList a]

      myLast :: [a] -> a
      myLast  = last

      myButLast :: [a] -> a
      myButLast = head . tail . reverse

      elementAt :: [a] -> Int -> a
      elementAt xs i = head (drop (i-1) xs)

      myLength :: [a] -> Int
      myLength = length

      isEqualIgnoreOrder :: Eq a => [a] -> [a] -> Bool
      isEqualIgnoreOrder xs ys = reverse xs == ys

      isPalindrome :: Eq a => [a] -> Bool
      isPalindrome xs = not isEven && isEqualIgnoreOrder take' drop'
                        where
                          isEven = even (length xs)
                          take'  = take (length xs `div` 2) xs
                          drop'  = drop (length xs `div` 2 + 1) xs
