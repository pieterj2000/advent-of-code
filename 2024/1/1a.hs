import Data.List (sort)



inputFile = "1a.input"

main = do
    input <- readFile inputFile
    print $ calc input


parse :: String -> ([Int], [Int])
parse input = 
    let ls = lines input
        ws = map words ls
        firstcol = map (read . head) ws :: [Int]
        secondcol = map (read . head . tail) ws :: [Int]
    in (firstcol,secondcol)


calc :: String -> Int
calc input = 
    let (l,r) = parse input
        l' = sort l
        r' = sort r
    in sum $ zipWith (\a b -> abs(a-b)) l' r'