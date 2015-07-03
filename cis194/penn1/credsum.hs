toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns
    | length ns `mod` 2 == 0 = doubleSkip ns
    | otherwise              = defaultSkip ns

doubleSkip  :: [Integer] -> [Integer]
doubleSkip []     = []
doubleSkip (n : ns) = 2 * n : defaultSkip ns

defaultSkip :: [Integer] -> [Integer]
defaultSkip (n : ns) = n : doubleSkip ns

sumDigits :: [Integer] -> Integer
sumDigits ns = foldr (\x y -> (x `div` 10) + (x `mod` 10) + y) 0 ns

validate :: Integer -> Bool
-- validate n = sumDigits . doubleEveryOther . toDigits n `mod` 10 == 0
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0
