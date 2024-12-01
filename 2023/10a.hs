-- stack script --resolver lts-21.22

import qualified Data.Array as A
import Data.Array ((!))

inputFile = "10a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = findLongestDist . parseInput


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