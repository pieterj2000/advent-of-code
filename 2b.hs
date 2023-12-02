
-- stack script --resolver lts-21.22

import Data.Char (isDigit, isSpace)
import Data.List (sortBy, elemIndex, find)
import Data.Maybe (fromJust, fromMaybe)

input = "2a.input"

main = do
    input <- readFile input
    print $ calc (12,13,14) input

calc :: (Int, Int, Int) -> String -> Int
calc available = sum . map (power . minimalSet . snd . parseLine) . lines

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c xs = let (left, right) = span (/=c) xs
                in left : splitOn c (iftail right)
                where iftail [] = []
                      iftail (x:xs) = xs

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseLine :: String -> (Int, [(Int, Int, Int)])
parseLine line =    let (start, rest) = span (/= ':') line
                        index = read $ dropWhile (not . isDigit) start
                        subsets = map parseSubset $ splitOn ';' (tail rest)
                    in (index, subsets)

colors = [" red", " green", " blue"]

parseSubset :: String -> (Int, Int, Int)
parseSubset line = let  colorTexts = map trimLeft $ splitOn ',' line
                        list = map (span isDigit) colorTexts
                        mlist = map (\(a,b) -> (read a :: Int, fromJust $ elemIndex b colors)) list
                        flist = [ fromMaybe (0, c) (find ((==c) . snd) mlist ) | c<-[0,1,2]]
                        [r,g,b] = map fst flist
                    in (r,g,b)

possible :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
possible (rt,gt,bt) (r,g,b) = (r <= rt) && (g <= gt) && (b <= bt)

maxT :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
maxT (rt,gt,bt) (r,g,b) = (max r rt, max g gt, max b bt)

minimalSet :: [(Int, Int, Int)] -> (Int, Int, Int)
minimalSet = foldr maxT (0, 0, 0)

power :: (Int, Int, Int) -> Int
power (r,g,b) = r * g * b