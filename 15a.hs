import Data.Char (ord)
import Data.List (foldl')

inputFile = "15a.input"

main = do
    input <- readFile inputFile
    print $ calc input

calc :: String -> Int
calc = sum . map hash . parseInput

hash :: String -> Int
hash = foldl' (\acc el -> (17*(acc + ord el)) `mod` 256) 0

parseInput :: String -> [String]
parseInput = splitBy (==',')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = let (sub, rest) = break p xs
                in sub : splitBy p (drop 1 rest)