-- stack script --resolver lts-21.22

import qualified Data.Array as A
import Data.Array ((!), (//))
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

inputFile = "10a.input"

main = do
    input <- readFile inputFile
    print $ calc input

calc :: String -> Int
calc = interiorPoints . partOfLoop . parseInput


data Tile = Vert | Hor | NE | NW | SE | SW | None | S deriving (Show, Eq)

readTile :: Char -> Tile
readTile '|' = Vert
readTile '-' = Hor
readTile 'L' = NE
readTile 'J' = NW
readTile '7' = SW
readTile 'F' = SE
readTile '.' = None
readTile 'S' = S
readTile _   = error "unkown tile"

printTile :: Tile -> Char
printTile Vert  = '│'
printTile Hor   = '─'
printTile NE    = '└'
printTile NW    = '┘'
printTile SW    = '┐'
printTile SE    = '┌'
printTile None  = '.'
printTile S     = 'S'

type Point = (Int, Int)
type Grid = A.Array Point Tile

parseInput :: String -> (Point, Grid)
parseInput input =  let ls = lines input
                        height = length ls
                        width = length $ head ls
                        vals = zipWith (\index val -> ( (mod index width, div index width) , readTile val) ) [0..] $ filter (/= '\n') input
                        start = fst . head . filter ((==S) . snd) $ vals
                    in (start, A.array ((0,0), (width - 1, height - 1)) vals)

data Dir = U | D | L | R deriving (Show, Eq)

dirs :: Tile -> [Dir]
dirs Vert   = [U, D]
dirs Hor    = [L, R]
dirs NE     = [U, R]
dirs NW     = [U, L]
dirs SW     = [D, L]
dirs SE     = [D, R]
dirs None   = error "trying to find direction from a ground tile"
dirs S      = [U, D, L, R]

move :: Point -> Dir -> Point
move (x,y) U = (x, y-1)
move (x,y) D = (x, y+1)
move (x,y) R = (x+1, y)
move (x,y) L = (x-1, y)

bordering :: Grid -> Point -> [Point]
bordering grid p | tile == S    = filter (\q -> p `elem` (bordering grid q)) $
                                  filter (\q -> grid ! q /= None) $
                                  filter (A.inRange (A.bounds grid)) $
                                  map (move p) $ dirs tile
                 | otherwise    = map (move p) $ dirs tile
                    where tile = grid ! p

stepFrom :: Grid -> Point -> Point -> Point
stepFrom grid prev cur = head . filter (/= prev) . bordering grid $ cur

walk :: Grid -> Point -> Point -> [Point]
walk grid prev cur = map fst $ iterate (\(prev, cur) -> (cur, stepFrom grid prev cur)) (prev, cur)


findLongestDist :: (Point, Grid) -> Int
findLongestDist (s, grid) = let [a,b] = bordering grid s
                            in snd $ head $ dropWhile (not . fst) $ drop 1 $ zipWith3 (\a b i -> (a == b, i)) (walk grid s a) (walk grid s b) [0..]



type GridBool = [[Bool]]

subs :: [Tile] -> [Tile] -> [Tile] -> [Tile]
subs find replace [] = []
subs find replace text | find `isPrefixOf` text = replace ++ (subs find replace $ fromJust $ stripPrefix find text)
                       | otherwise = head text : (subs find replace $ tail text)

subTile :: [Tile] -> [Tile]
subTile =   subs [NE, NW] [] .
            subs [NE, SW] [Vert] .
            subs [SE, NW] [Vert] .
            subs [SE, SW] [] .
            subs [Hor] []



partOfLoop :: (Point, Grid) -> GridBool
partOfLoop (s, grid) =  let (q:_) = bordering grid s
                            loop = s : (takeWhile (/= s) $ drop 1 $ walk grid s q)
                            (_,(xB,yB)) = A.bounds grid

                            subS :: Tile -> Tile
                            subS S = let rGrid t = grid // [(s, t)]
                                      in head $ filter (\t -> bordering (rGrid t) s == bordering grid s) [Hor, Vert, NW, NE, SW, SE]
                            subS t = t

                            g = [ [ if (x,y) `elem` loop then grid ! (x,y) else None | x<-[0..xB] ] | y<-[0..yB] ]
                            g' = map (map subS) g 

                            in map (map (/=None) . subTile) g'


interiorLine :: [Bool] -> Int
interiorLine line = let nxt elInLoop (_, acc) = (acc && not elInLoop, acc')
                                where acc' = acc /= elInLoop
                      in length $ filter id $
                        map fst $
                        scanr nxt (False, False)
                        line

interiorPoints :: GridBool -> Int
interiorPoints grid = sum (map interiorLine grid)

printLoop :: (Point, Grid) -> String
printLoop (s, grid) =   let (q:_) = bordering grid s
                            loop = s : (takeWhile (/= s) $ drop 1 $ walk grid s q)
                            (_,(xB,yB)) = A.bounds grid
                            g = [ [ if (x,y) `elem` loop then grid ! (x,y) else None | x<-[0..xB] ] | y<-[0..yB] ]
                        in unlines $ map (map printTile) g