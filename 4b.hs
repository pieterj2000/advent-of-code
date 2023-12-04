
-- stack script --resolver lts-21.22
import qualified Parser as P
import Control.Applicative (Alternative(..))
import Data.List (intersect, foldl')


import Debug.Trace


inputFile = "4a.input"

main = do
    input <- readFile inputFile
    print $ calc input

type Line = (Int, [Int], [Int])

calc :: String -> Int
calc = iter 0 (repeat 1) . map (calcLine . parseLine) . lines

parseLine :: String -> Line
parseLine = let cardNumber = P.string "Card" *> some P.space *> P.int <* P.char ':'
                number = many P.space *> P.int
                sequence = some number
                separator = many P.space *> P.char '|' <* many P.space
                parser = (\a b _ c -> (a,b,c)) <$> cardNumber <*> sequence <*> separator <*> sequence
            in P.parseResult parser

calcLine :: Line -> Int
calcLine (_,v,w) = length $ intersect v w

iter :: Int -> [Int] -> [Int] -> Int
iter tot _ [] = tot
iter tot (c:cs) (w:ws) = let have i = if w >= i then c else 0
                             newcs = zipWith (+) cs (map have [1..])
                    in iter (tot + c) newcs ws

{-
iter :: [Int] -> Int
iter = (\(a,_,_,_,_,_ ) -> a) . foldl' step (0,1,1,1,1,1)

step :: (Int, Int, Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int, Int, Int)
step (tot, a,b,c,d,e) points = let p i points = if points >= i then a else 0
                          in (tot + a, b + p 1 points, c + p 2 points, d + p 3 points, e + p 4 points, 1 + p 5 points)

-}