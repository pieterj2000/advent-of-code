
-- stack script --resolver lts-21.22

import Data.Char (isDigit, isSpace)
import Data.List (sortBy, elemIndex, find)
import Data.Maybe (fromJust, fromMaybe)

import qualified Parser as P

import Control.Applicative (Alternative (..))

input = "2a.input"

main = do
    input <- readFile input
    print $ calc (12,13,14) input

calc :: (Int, Int, Int) -> String -> Int
calc available = sum . map fst . filter (all (possible available) . snd) . map parseLine . lines

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c xs = let (left, right) = span (/=c) xs
                in left : splitOn c (iftail right)
                where iftail [] = []
                      iftail (x:xs) = xs

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseLine :: String -> (Int, [(Int, Int, Int)])
parseLine line = let    gameNum = P.string "Game " *> P.int <* P.char ':'
                        rp = (\a -> (a,0)) <$> (many (P.char ',') *> many P.space *> P.int <* P.string " red")
                        gp = (\a -> (a,1)) <$> (many (P.char ',') *> many P.space *> P.int <* P.string " green")
                        bp = (\a -> (a,2)) <$> (many (P.char ',') *> many P.space *> P.int <* P.string " blue")
                        subset = some (rp <|> gp <|> bp) <* many (P.char ';')
                        parser = (\a b -> (a,b)) <$> gameNum <*> some subset
                        (num, mlist) = P.parseResult parser line

                        fillNsort clist = [ fromMaybe (0, c) (find ((==c) . snd) clist ) | c<-[0,1,2]]
                        tuplize = (\[r,g,b] -> (r,g,b)) . map fst

                    in (num, map (tuplize . fillNsort) mlist)

possible :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
possible (rt,gt,bt) (r,g,b) = (r <= rt) && (g <= gt) && (b <= bt)