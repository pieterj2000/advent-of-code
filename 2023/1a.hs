#!/usr/bin/env stack
-- stack script --resolver lts-21.22
import Data.Char (isDigit)

input = "1a.input"

main = do
    input <- readFile input

    print $ calc input


calc :: String -> Int
calc = sum . map calcLine . lines


calcLine :: String -> Int
calcLine line = let s = filter isDigit line
                    tup = [head s, last s]
                in read tup :: Int