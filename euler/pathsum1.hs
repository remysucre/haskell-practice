import Data.Functor
import Control.Applicative
import System.Environment

toNums :: String -> [[Int]]
toNums grid = map ((map read) . words) $ lines grid

largep :: String -> Int
largep grid = maximum $ foldl maxpath [0] $ toNums grid

maxpath :: [Int] -> [Int] -> [Int]
maxpath row prev = map maxp row
    where maxp n = maximum $ map (+ n) prev

main = do 
    fc <- head <$> getArgs >>= readFile
    let res = largep fc
    putStr $ show res
