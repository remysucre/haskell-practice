nextsteps :: Int -> (Int, Int) -> [(Int, Int)]
nextsteps n (x, y)
    | x < n && y < n = [(x + 1, y), (x, y + 1)]
    | x == n && y == n = [(x, y)]
    | x == n && y < n = [(x, y + 1)]
    | x < n && y == n = [(x + 1, y)]

paths 0 0 = 1
paths x 0 = paths (x - 1) 0
paths 0 y = paths 0 (y - 1)
paths x y = paths (x - 1) y + paths x (y - 1)

paths' 0 _ ps = ps
paths' n e ps = paths' (n - 1) e $! (ps >>= nextsteps e)

ans = length $ paths' 20 10 [(0, 0)]
