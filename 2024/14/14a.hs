{-# LANGUAGE DataKinds #-}
import Data.Mod.Word
import Linear.V2
import Data.Char (isDigit, isSpace)
import Linear ((*^))

(xm,ym) = if inputFile == "14a.input" then (101,103) else (11,7)

inputFile = "14a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

type V = V2 Int

parse :: String -> [(V,V)]
parse = map doel . lines
    where
        doel = (\[a,b,c,d] -> (V2 a b, V2 c d)) . map read . words . filter (\c -> isDigit c || isSpace c || c == '-') . map sub 
        sub ',' = ' '
        sub c = c

vmod :: V -> V
vmod (V2 x y) = V2 (x `mod` xm) (y `mod` ym)

attime :: Int -> (V,V) -> V
attime t (x,dx) = vmod $ x + t *^ dx

inQuadrant :: (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> V -> Bool
inQuadrant o1 o2 (V2 x y) = x `o1` xsplit && y `o2` ysplit
    where 
        xsplit = (xm - 1) `div` 2
        ysplit = (ym - 1) `div` 2

safetyFactor :: [V] -> Int
safetyFactor vs = let ops = [(<),(>)] in product [ length $ filter (inQuadrant o1 o2) vs | o1 <-ops, o2<-ops ]

--calc :: [(V,V)] -> Int
calc = safetyFactor . map (attime 100)

