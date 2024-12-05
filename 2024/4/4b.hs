import Data.List (tails, transpose)

inputFile = "4a.input"

main = do
    input <- readFile inputFile
    print $ calc . parse $ input


parse :: String -> [[Char]]
parse = lines

possibilities :: [[a]] -> [[a]]
possibilities = map maakblok . concatMap vheadblok . transpose . map (takeWhile ((>= 3) . length) . tails)
    where
        headblok = concat . map (take 3) . take 3
        vheadblok = map headblok . takeWhile ((>= 3) . length) . tails

maakblok :: [a] -> [a]
maakblok [] = []
maakblok [x] = [x]
maakblok (x:y:xs) = x : maakblok xs

calc :: [[Char]] -> Int
calc = length . filter (`elem` ["MSAMS", "MMASS", "SMASM", "SSAMM"]) . possibilities