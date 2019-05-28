module ThrowDice
    (throwDice) where

{--
This problem was asked by Spotify.

Write a function, throw_dice(N, faces, total), that determines
how many ways it is possible to throw N dice with some number of faces each to
get a specific total.

For example, throw_dice(3, 6, 7) should equal 15.
15
=
. length
[7,7,7,7,7,7,7,7,7,7,7,7,7,7,7]
. filter (==total)
[3, 4, 5, n + n + n]
. map sum
[[1,1,1][1,1,2][1,1,3] .. [n,n,n]] (cartesian product)
. sequence
[[1,2,3,4,5,6],[1,2,3,4,5,6],[1,2,3,4,5,6]]
$ dices n faces
--}

throwDice               :: Int -> Int -> Int -> Int
throwDice n faces total = length
                           . filter (== total)
                           . map sum
                           . sequence $ dices n faces

dices         :: Int -> Int -> [[Int]]
dices n faces = replicate n [1..faces]
