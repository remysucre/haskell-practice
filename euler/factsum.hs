import Data.List
import Data.Char

ans = sum $ map digitToInt $ show $ foldr (*) 1 [1..100]
