import Data.List

subs = repeat [1..500]

triangles = [n * (n + 1) `div` 2 | n <- [1..], factors n > 5]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromInt

factors :: Int -> Int
factors n = 2 * length facs
    where facs = takeWhile (<= intSqrt n) [x | x <- [1..], n `mod` x == 0]

ans = takeWhile (\x -> factors x /= 1) triangles
