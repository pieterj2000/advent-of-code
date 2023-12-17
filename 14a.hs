import Data.List (transpose)


inputFile = "14a.input"

main = do
    input <- readFile inputFile
    print $ calc input

type Row = String
type SubRow = (String, Int)

calc :: String -> Int
calc = sum . map doRow . parseInput

parseInput :: String -> [Row]
parseInput = transpose . lines

doRow :: Row -> Int
doRow = sum . map doSubRow . splitRow

doSubRow  :: SubRow -> Int
doSubRow (xs, i) = sum $ take (numberRocks xs) [i,(i-1)..]

numberRocks :: Row -> Int
numberRocks = length . filter (=='O')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = let (sub, rest) = break p xs
                in sub : splitBy p (drop 1 rest)

hasRocks :: Row -> Bool
hasRocks = elem 'O'

splitRow :: Row -> [SubRow]
splitRow r =    let len = length r
                    subs = splitBy (\(i, el) -> el == '#') $ zip [len, (len-1)..] r
                    subs' = filter (hasRocks . map snd) subs
                in map (\xs -> (map snd xs, fst $ head xs)) subs'