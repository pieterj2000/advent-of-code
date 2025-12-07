import Data.List (transpose, foldl1')

inputFile = "6a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [([Int], Int -> Int -> Int)]
parse spul = 
    let ls = lines spul
        getallen = sep $ transpose $ init ls
        p "+" = (+)
        p "*" = (*)
        ops = map p $ words $ last ls
    in zip getallen ops

sep [] = []
sep xs = let (l, rest) = span (not . all (==' ')) xs in (map read l) : sep (drop 1 rest)

calc = sum . map doe


doe (getallen, op) = foldl1' op getallen 