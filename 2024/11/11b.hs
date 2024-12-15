import qualified Data.IntMap as M

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


tickgen :: M.IntMap Int -> M.IntMap Int
tickgen = M.foldrWithKey' f M.empty
    where
        f :: Int -> Int -> M.IntMap Int -> M.IntMap Int
        f val num = g (tick val)
            where
                g :: [Int] -> M.IntMap Int -> M.IntMap Int
                g [] m = m
                g (x:xs) m = g xs (M.insertWith (+) x num m)


ticky :: Int -> M.IntMap Int -> Int
ticky 0 m = sum m
ticky t m = ticky (t-1) (tickgen m)

calc :: [Int] -> Int
calc input = ticky 75 $ M.fromList $ map (\x -> (x,1)) input