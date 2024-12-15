import Linear.V2
import Data.Char (isSpace, isDigit)
import Linear (dot)
import Data.Maybe (mapMaybe)


inputFile = "13a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [(V2 Int, V2 Int, V2 Int)]
parse = doe . filter (not . null) . lines
    where
        doe [] = []
        doe (l1:l2:l3:ls) = (pline l1, pline l2, pline l3) : doe ls
        pline = (\[a,b] -> V2 a b) . map read . words . filter (\c -> isSpace c || isDigit c)

ispossible :: (V2 Int, V2 Int, V2 Int) -> Bool
ispossible (V2 x1 y1, V2 x2 y2, V2 xt yt) = (xt `mod` xd == 0) && (yt `mod` yd == 0)
    where
        xd = gcd x1 x2
        yd = gcd y1 y2

calcbuttonpresses :: (V2 Int, V2 Int, V2 Int) -> Maybe (V2 Int)
calcbuttonpresses (V2 x1 y1, V2 x2 y2, V2 xt yt)
    | r1 ==0 && r2 == 0 = Just $ V2 a b
    | otherwise = Nothing
    where
        (a, r1) = (10000000000000 + xt - b*x2) `divMod` x1
        (b, r2) = (x1*(yt + 10000000000000) - y1*(xt + 10000000000000)) `divMod` (x1*y2-x2*y1)

calccost ::  V2 Int -> Int
calccost t = V2 3 1 `dot` t

fewbuttons :: V2 Int -> Bool
fewbuttons (V2 x y) = x <= 100 && y <= 100 && x >= 0 && y >= 0

--calc :: [(V2 Int, V2 Int, V2 Int)] -> Int
calc = sum . map calccost . mapMaybe calcbuttonpresses
