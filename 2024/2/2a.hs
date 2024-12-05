
inputFile = "2a.input"

main = do
    input <- readFile inputFile
    print $ calc . parse $ input


parse :: String -> [[Int]]
parse = map (map read . words) . lines

calc :: [[Int]] -> Int
calc reports = length $ filter (isSafe . increments) reports
    where
        increments levels = zipWith (-) (tail levels) levels
        isSafe incrs = (all (\i -> i < 0 && i > -4) incrs) || (all (\i -> i > 0 && i < 4) incrs )