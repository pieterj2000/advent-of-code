-- stack script --resolver lts-21.22

import qualified Data.Array as A
import Data.Array ((!))
import Data.Maybe (fromJust)
import Data.List (elemIndex, foldl1')

inputFile = "8a.input"

main = do
    input' <- readFile inputFile
    let repl '1' = 'P'
        repl '2' = 'Q'
        repl c = c
    let input = if inputFile == "8b.ex.input" then map repl input' else input'
    --print $ calc input
    calc input

--calc :: String -> Int
--calc = length . takeWhile (not . all isZ) . walkPath . parseInput
calc = mapM_ (print . map intToLabel) . take 20 . takeWhile (not . all isZ) . walkPath . parseInput

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

isA :: Int -> Bool
isA n = (n `mod` 26) == 0

isZ :: Int -> Bool
isZ n = (n `mod` 26) == 25

parseInput :: String -> (A.Array Int (Int, Int), [Direction], [Int])
parseInput input =  let dir = map (read . pure) . head . lines $ input
                        elts = map parseLine $ drop 2 $ lines input
                        startNodes = filter isA $ map fst elts
                        ar = A.array (0, 26*26*26-1) elts
                    in (ar, dir, startNodes)

doStep :: A.Array Int (Int, Int) -> ([Int],[Direction]) -> ([Int],[Direction])
doStep graph (curNodes, d:dirs) =   let step place = (if d == L then fst else snd) (graph ! place)
                                        nextNodes = map step curNodes
                                    in (nextNodes, dirs)

walkPath :: (A.Array Int (Int, Int), [Direction], [Int]) -> [[Int]]
walkPath (graph, dirs, startNodes) = map fst $ iterate (doStep graph) (startNodes, cycle dirs)
