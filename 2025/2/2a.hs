import Data.Char (isDigit)

inputFile = "2a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input
    -- mapM_ print $ calc . parse $ input

parse :: String -> [(Int, Int)]
parse [] = []
parse spul = 
    let (links, rest1) = span isDigit spul
        (rechts, rest2) = span isDigit $ dropWhile (not . isDigit) rest1
        tup = (read links, read rechts)
    in tup : parse (dropWhile (not . isDigit) rest2)

calc :: [(Int, Int)] -> Int
calc = sum . map counttup
-- calc = map (\x -> (x, gentup x))


counttup :: (Int, Int) -> Int
counttup = sum . gentup

enums :: String -> [Int]
enums l = map (read . (\x -> x ++ x) . show) [(read l :: Int)..]

gentup :: (Int, Int) -> [Int]
gentup (l, r) =
    let sl = show l
        (lenl, remain) = (length sl) `divMod` 2
        start | remain == 0 = take lenl sl
              | otherwise   = '1' : replicate lenl '0'

        invalids = takeWhile (<= r) $ dropWhile (< l) $ enums start
    in invalids