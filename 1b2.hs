#!/usr/bin/env stack
-- stack script --resolver lts-21.22

import qualified Parser as P
import Control.Applicative (asum, (<|>), some)


input = "1a.input"

main = do
    input <- readFile input
    print $ calc input


calc :: String -> Int
calc = sum . map calcLine . lines
digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
rdigits = map reverse digits

digitText :: P.Parser Int
digitText = let p s i = P.string s *> pure i
            in asum (zipWith p digits [1..])

rdigitText :: P.Parser Int
rdigitText = let p s i = P.string s *> pure i
            in asum (zipWith p rdigits [1..])

digitSequence :: P.Parser [Int]
digitSequence = let p = digitText <|> P.digit <|> (P.anyChar *> p)
                in some p

rdigitSequence :: P.Parser [Int]
rdigitSequence = let p = rdigitText <|> P.digit <|> (P.anyChar *> p)
                in some p

calcLine :: String -> Int
calcLine line = let l = head $ P.parseResult digitSequence line
                    r = head $ P.parseResult rdigitSequence $ reverse line
                in l*10+r