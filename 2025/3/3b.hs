import Data.Char (digitToInt)

inputFile = "3a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

--calc :: [[Int]] -> Int
calc = sum . map (doe 0 12)

doe :: Int -> Int -> [Int] -> Int
doe acc 0 _ = acc
doe acc n xs = 
    let len = length xs
        getal = maximum $ take (len - n + 1) xs
        rest = tail $ dropWhile (/=getal) xs
        acc' = acc * 10 + getal
    in doe acc' (n-1) rest