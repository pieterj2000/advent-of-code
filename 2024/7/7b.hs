
inputFile = "7a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [(Int, [Int])]
parse = map doel . lines
    where
        doel l = (read $ takeWhile (/=':') l, map read $ words $ tail $ dropWhile (/= ':') l)

calc :: [(Int, [Int])] -> Int
calc = sum . map fst . filter ismaakbaar

ismaakbaar :: (Int, [Int]) -> Bool
ismaakbaar (doel, xs) = doel `elem` possibleVals xs

numDigits :: Int -> Int
numDigits x = 1 + floor (logBase 10 (fromIntegral x))

(.||) :: Int -> Int -> Int
a .|| b = a*m+b
    where
        m = 10^(numDigits b)


apply :: (Int -> Int -> Int) -> [Int] -> [Int]
apply operator (x:y:xs) = operator x y : xs

possibleVals :: [Int] -> [Int]
possibleVals [x] = [x]
possibleVals xs = do
    op <- [(*), (+), (.||)]
    let nieuw = apply op xs
    possibleVals nieuw