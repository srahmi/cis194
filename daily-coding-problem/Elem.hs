module Elem
    (elem') where

{--
This problem was asked by Netflix.

Given a sorted list of integers of length N, determine if an element x is in
the list without performing any multiplication, division, or bit-shift operations.
--}

elem'   :: Eq a => a -> [a] -> Bool
elem' e = foldr (\x acc -> (x == e) || acc) False
