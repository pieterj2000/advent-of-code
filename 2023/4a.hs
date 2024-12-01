
-- stack script --resolver lts-21.22
import qualified Parser as P
import Control.Applicative (Alternative(..))
import Data.List (intersect)


import Debug.Trace


inputFile = "4a.input"

main = do
    input <- readFile inputFile
    print $ calc input

type Line = (Int, [Int], [Int])

calc :: String -> Int
calc = sum . map (calcLine . parseLine) . lines


parseLine :: String -> Line
parseLine = let cardNumber = P.string "Card" *> some P.space *> P.int <* P.char ':'
                number = many P.space *> P.int
                sequence = some number
                separator = many P.space *> P.char '|' <* many P.space
                parser = (\a b _ c -> (a,b,c)) <$> cardNumber <*> sequence <*> separator <*> sequence
            in P.parseResult parser

calcLine :: Line -> Int
calcLine (_,v,w) = let matching = length $ intersect v w
                    in if matching == 0 then 0 else 2^(matching - 1)
