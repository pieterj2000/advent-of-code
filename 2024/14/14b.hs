{-# LANGUAGE DataKinds #-}
import Data.Mod.Word
import Linear.V2
import Data.Char (isDigit, isSpace)
import Linear ((*^), Metric (qd))
import Data.List (sort, intersperse, intercalate, sortOn)

(xm,ym) = if inputFile == "14a.input" then (101,103) else (11,7)

inputFile = "14a.input"
main = do
    input <- readFile inputFile
    putStrLn $ calc . parse $ input

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

printspul :: [V] -> String
printspul vs = unlines $ map doel [0..(ym-1)]
    where
        doel y = map (\x -> if (V2 x y) `elem` vs then 'X' else '.' ) [0..(xm-1)]


testval :: [V] -> Int
testval = sum . map (qd (V2 xmid ymid))
    where
        xmid = (xm - 1) `div` 2
        ymid = (ym - 1) `div` 2

getmintime :: [(V,V)] -> (Int,[V])
getmintime inp = head $ sortOn (testval . snd) lijst
    where
        lijst = map (\i -> (i, map (attime i) inp)) [0..(xm*ym)]

--calc :: [(V,V)] -> Int
calc = (\(i,v) -> show i ++ "\n" ++  printspul v) . getmintime

