

inputFile = "12.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = sum . map (\(s,c) -> count s ((-1):c)) . parseInput


parseRight :: String -> [Int]
parseRight [] = []
parseRight xs = read (takeWhile (/=',') xs) : parseRight (tailif $ dropWhile (/=',') xs)
    where tailif [] = []
          tailif (x:xs) = xs

parseInput :: String -> [(String, [Int])]
parseInput = map (\l -> (takeWhile (/=' ') l, parseRight $ tail $ dropWhile (/=' ') l) ) . lines


count :: String -> [Int] -> Int
count [] [0] = 1
count [] [-1] = 1
count [] cs = if all (<=0) cs then 1 else 0
count ms [0] = if all (`elem` ".?") ms then 1 else 0
count ms [-1] = if all (=='.') ms then 1 else 0
count (m:ms) (c:cs) | m == '.' && c > 0     = 0                     -- still needs #, so error
                    | m == '.' && c == 0    = count ms ((c-1):cs)   -- first . after #
                    | m == '.' && c == -1   = count ms (c:cs)       -- . somewhere not interesting
                    | m == '#' && c > 0     = count ms ((c-1):cs)   -- in # sequence
                    | m == '#' && c == 0    = 0                     -- just finished # sequence, so too many #'s
                    | m == '#' && c == -1   = count (m:ms) cs       -- start new sequence
                    | m == '?' && c > 0     = count ms ((c-1):cs)   -- busy with # sequence, so consider as #
                    | m == '?' && c == 0    = count ms ((c-1):cs)   -- just finished # sequence, so must be .
                    | m == '?' && c == -1   = count ms (c:cs) +     -- consider as . and continue
                                              count (m:ms) cs       -- consider as # and start new sequence