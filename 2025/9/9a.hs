

import Data.List (maximumBy)
import Data.Ord (comparing)


inputFile = "9a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [(Int, Int)]
parse = map ((\[a,b] -> (a,b)) . take 2 . map read . words . map (\c -> if c==',' then ' ' else c)) . lines

--calc :: [[Int]] -> Int
calc punten = 
    let hoeken = [ (a,b) | a <- punten, b <- punten, a < b]
        beste = maximumBy (comparing area) hoeken

    in area beste

area ((x1,y1),(x2,y2)) = (1 + abs (x1-x2)) * (1 + abs (y1 - y2))

