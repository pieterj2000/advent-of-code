
-- stack script --resolver lts-21.22
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf,stripPrefix)
import Data.Maybe (isJust, fromJust)

input = "1a.input"

main = do
    input <- readFile input

    print $ calc input


calc :: String -> Int
calc = sum . map calcLine . lines


calcLine :: String -> Int
calcLine line = let (_,s) = getSequence (line,[])
                    (_,l) = (head s, last s)
                    (_,sr) = rgetSequence (reverse line,[])
                    (_,r) = (head sr, last sr)
                in l*10+r

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
rdigits = map reverse digits


getSequence :: (String, [Int]) -> (String, [Int])
getSequence ([], x) = ([], x)
getSequence (line, list)
    | any (`isPrefixOf` line) digits = let  x = map (`stripPrefix` line) digits
                                            y = zip x [1..]
                                            (s,d) = head $ filter (isJust . fst) y
                                            in getSequence (fromJust s, d:list)
    | isDigit (head line)            = let d = digitToInt $ head line in getSequence (tail line, d:list)
    | otherwise = getSequence (tail line, list)
    
rgetSequence :: (String, [Int]) -> (String, [Int])
rgetSequence ([], x) = ([], x)
rgetSequence (line, list)
    | any (`isPrefixOf` line) rdigits = let x = map (`stripPrefix` line) rdigits
                                            y = zip x [1..]
                                            (s,d) = head $ filter (isJust . fst) y
                                            in rgetSequence (fromJust s, d:list)
    | isDigit (head line)            = let d = digitToInt $ head line in rgetSequence (tail line, d:list)
    | otherwise = rgetSequence (tail line, list)