import Data.Char (digitToInt)

inputFile = "3a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

--calc :: [[Int]] -> Int
calc = sum . map doe

doe :: [Int] -> Int
doe xs = 
    let eerste = maximum $ init xs
        tweede = maximum . tail $ dropWhile (/=eerste) xs
    in eerste*10+tweede