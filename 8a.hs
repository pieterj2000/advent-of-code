-- stack script --resolver lts-21.22

import qualified Data.Array as A
import Data.Array ((!))
import Data.Maybe (fromJust)
import Data.List (elemIndex, foldl1')

inputFile = "8a.input"

main = do
    input <- readFile inputFile
    print $ calc input

calc :: String -> Int
calc = length . takeWhile (/= (26*26*26-1)) . walkPath . parseInput

data Direction = L | R deriving (Read, Show, Eq)

intToLabel :: Int -> String
intToLabel n = map (['A'..'Z'] !!) [a,b,c]
    where   a = n `div` (26*26)
            b = (n `mod` (26*26)) `div` 26
            c = n `mod` 26

labelToInt :: String -> Int
labelToInt = foldl1' (\acc x -> 26*acc + x)  . map (fromJust . flip elemIndex ['A'..'Z'])

parseLine :: String -> (Int, (Int, Int))
parseLine line = (labelToInt label, (labelToInt l, labelToInt r))
    where   label = take 3 line
            l = take 3 $ drop 7 line
            r = take 3 $ drop 12 line

parseInput :: String -> (A.Array Int (Int, Int), [Direction])
parseInput input =  let dir = map (read . pure) . head . lines $ input
                        elts = map parseLine $ drop 2 $ lines input
                        ar = A.array (0, 26*26*26-1) elts
                    in (ar, dir)

doStep :: A.Array Int (Int, Int) -> (Int,[Direction]) -> (Int,[Direction])
doStep graph (place, d:dirs) =    let nextplace = (if d == L then fst else snd) (graph ! place)
                                  in (nextplace, dirs)

walkPath :: (A.Array Int (Int, Int), [Direction]) -> [Int]
walkPath (graph, dirs) = map fst $ iterate (doStep graph) (0, cycle dirs)
