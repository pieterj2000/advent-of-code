
inputFile = "11a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [Int]
parse = map read . words

numDigits :: Int -> Int
numDigits x = 1 + floor (logBase 10 (fromIntegral x))

tick :: Int -> [Int]
tick 0 = [1]
tick i
    | even (numDigits i)    = let d = 10^(numDigits i `div` 2) in [i `div` d, i `mod` d]
    | otherwise             = [i*2024]


ticky :: Int -> Int -> Int
ticky 0 _ = 1
ticky t i = sum $ map (ticky (t-1)) $ tick i

calc :: [Int] -> Int
calc = sum . map (ticky 25)