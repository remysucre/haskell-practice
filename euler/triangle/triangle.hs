import Data.List

triangles :: [Int]
triangles = [n * (n + 1) `div` 2 | n <- [1..]]
triangles'' = takeWhile (/= 0) [n * (n + 1) `div` 2 | n <- [1..]]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

factors :: Int -> Int
factors n = 2 * length facs
    where facs = [x | x <- [1..intSqrt n], n `mod` x == 0]

ans = [x | x <- triangles, factors x >= 500]
