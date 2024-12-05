
inputFile = "2a.input"

main = do
    input <- readFile inputFile
    print $ diffs . parse $ input


parse :: String -> [[Int]]
parse = map (map read . words) . lines

isSafe :: [Int] -> Bool
isSafe incrs
    | not $ p (head incs)   = all p (tail incs) || doe incs
    | otherwise             = doe incs
    where
        isnegative = length (filter (<0) incrs) >= 3
        incs = if isnegative then map (*(-1)) incrs else incrs
        p i = i > 0 && i < 4
        doe [] = True
        doe [x] = True
        doe (x:y:xs)
            | p x       = doe (y:xs)
            | otherwise = all p $ (x+y):xs


choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs


isSafe' :: [Int] -> Bool
isSafe' vals = 
    let ls = choose (length vals - 1) vals
        check l = let incs = zipWith (-) (tail l) l in (all (\i -> i < 0 && i > -4) incs) || (all (\i -> i > 0 && i < 4) incs )
    in any check ls


calc :: [[Int]] -> Int
calc reports = length $ filter (isSafe . increments) reports
    where
        increments levels = zipWith (-) (tail levels) levels

calc' :: [[Int]] -> Int
calc' reports = length $ filter (isSafe') reports
    where
        increments levels = zipWith (-) (tail levels) levels

diffs :: [[Int]] -> [[Int]]
diffs = filter (\r -> calc [r] /= calc' [r])