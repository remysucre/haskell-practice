skips :: [a] -> [[a]]
skips xs = 

nSkip n xs = 

indexOf :: Int Int -> Bool
indexOf e n = e `mod` n == 0

skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == []

