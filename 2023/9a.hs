-- stack script --resolver lts-21.22


inputFile = "9a.input"

main = do
    input <- readFile inputFile
    print $ calc input

calc :: String -> Int
calc = sum . map next . parseInput 
--calc = sum . map (next' . reverse) . parseInput


parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

next :: [Int] -> Int
next xs = lagrange xs (length xs)

-- reverse list als input graag
next' :: [Int] -> Int
next' [] = error "Not enough data"
next' xs | all (==0) xs = 0
         | otherwise    = next' diffseq + head xs
            where diffseq = zipWith (-) xs (tail xs)


lagrange :: [Int] -> (Int -> Int)
lagrange vals x =   let l :: Int -> Double
                        l k = product $ map (\i -> (fromIntegral (x-i)) / (fromIntegral (k-i)) ) $ filter (/= k) [0..(length vals - 1)]
                    in round $ sum $ zipWith (\index val -> (fromIntegral val) * l index) [0..] vals