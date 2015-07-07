module Golf where

import Data.List

-- exercise 1
indices = map (\n -> iterate (+n) n) [1..]

skipIndices n = map (takeWhile (<= n)) $ take n indices

elmsAt xs ns = map (\n -> xs !! (n - 1)) ns

skips :: [a] -> [[a]]
skips xs = map (elmsAt xs) $ skipIndices (length xs)

-- exercise 2

localMaxima (x:rest@(y:z:_))
  | y > x && y > z = y : localMaxima rest
  | otherwise      = localMaxima rest
localMaxima _ = []

-- exercise 3

-- bad exercise
