import Data.Char (isDigit)
import Data.List (nub)

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

--calc :: [(Int, Int)] -> Int
-- calc = sum . map counttup
calc = sum . concat . map pertup
-- calc = map (\x -> (x, gentup x))


pertup :: (Int, Int) -> [Int]
pertup dinges@(l, r) =
    let lenl = length $ show l
        lenr = length $ show r
        ds = mogelijkedinges (lenl, lenr)
    in nub $ concatMap (gentup dinges) ds

enums :: Int -> String -> [Int]
enums delen l = map (read . (\x -> concat $ replicate delen x) . show) [(read l :: Int)..]

gentup :: (Int, Int) -> Int -> [Int]
gentup (l, r) delen =
    let sl = show l
        (lenl, remain) = (length sl) `divMod` delen
        start | remain == 0 = take lenl sl
              | otherwise   = '1' : replicate lenl '0'

        invalids = takeWhile (<= r) $ dropWhile (< l) $ enums delen start
    in invalids



divisors :: Int -> [Int]
divisors x = [ d | d<-[2..x], x `mod` d == 0]
    where
        len = length $ show x
        ub = x `div` 2

mogelijkedinges :: (Int, Int) -> [Int]
mogelijkedinges (l, r) = nub $ divisors l ++ divisors r