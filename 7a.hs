-- stack script --resolver lts-21.22

import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromJust)
import Data.List (elemIndex, sort, group, sortOn)

inputFile = "7a.input"

main = do
    input <- readFile inputFile
    print $ calc input

calc :: String -> Int
calc = totalWinnings . sortByStrength . parseInput


type Hand = [Int]

parseCard :: Char -> Int
parseCard c | c == '0' || c == '1' = error "geen 1 of 0 als kaart kan"
            | isDigit c = digitToInt c
            | otherwise = 10 + fromJust (elemIndex c "TJQKA")

parseLine :: String -> (Hand, Int)
parseLine line = (map parseCard $ take 5 line, read $ drop 6 line)

parseInput :: String -> [(Hand, Int)]
parseInput = map parseLine . lines

scoreHand :: Hand -> [Int]
scoreHand hand = reverse . (reverse hand ++) . sort . map length . group . sort $ hand

sortByStrength :: [(Hand, Int)] -> [(Hand, Int)]
sortByStrength = sortOn (scoreHand . fst)

totalWinnings :: [(Hand, Int)] -> Int
totalWinnings = sum . zipWith (*) [1..] . map snd