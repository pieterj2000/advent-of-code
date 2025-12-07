import Data.List (transpose, foldl1')

inputFile = "6a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [([Int], Int -> Int -> Int)]
parse spul = 
    let ls = map words $ lines spul
        getallen = map (map read) $ init ls
        p "+" = (+)
        p "*" = (*)
        ops = map p $ last ls
    in zip (transpose getallen) ops

calc = sum . map doe


doe (getallen, op) = foldl1' op getallen 