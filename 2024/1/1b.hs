import Data.List (sort, group)
import Data.Maybe (fromMaybe)



inputFile = "1a.input"

main = do
    input <- readFile inputFile
    print $ calc input


parse :: String -> ([Int], [Int])
parse input =
    let ls = lines input
        ws = map words ls
        firstcol = map (read . head) ws :: [Int]
        secondcol = map (read . head . tail) ws :: [Int]
    in (firstcol,secondcol)

calcFreq :: [Int] -> [(Int, Int)]
calcFreq = map (\xs -> (head xs, length xs)) . group . sort

calc :: String -> Int
calc input =
    let (l,r) = parse input
        freqs = calcFreq r
    in sum $ map (\i -> fromMaybe 0 (lookup i freqs) * i) l