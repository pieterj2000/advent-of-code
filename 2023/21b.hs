import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import qualified Data.Set as S
import Data.Set ((\\))
import Debug.Trace

inputFile = "21a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = totaal . fst . parseInput

type Point = (Int, Int)
type Grid = A.Array Point Bool
type Points = S.Set Point

isFarm :: Grid -> Point -> Bool
isFarm = (!)

inGrid :: Grid -> Point -> Bool
inGrid g = A.inRange (A.bounds g)

neighbours :: Grid -> Point -> Points
neighbours g (x,y) = S.fromList . filter (isFarm g) . filter (inGrid g) $ nbs
    where nbs = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

doStep :: Grid -> (Points, Points) -> (Points, Points)
doStep g (points, boundary) =
    let neighb = S.unions $ S.map (neighbours g) boundary
        newBoundary = neighb \\ points
        newPoints = S.union neighb points
    in (newPoints, newBoundary)

calcAmount :: Bool -> Int -> (Grid, Point) -> Int
calcAmount parity minDist (g, start) =
    let startSet = S.singleton start
        steps = zip ([0..] :: [Int]) $ map snd $ iterate (doStep g) (startSet, startSet)
        goeieSteps = filter (\(dist,pts) -> even dist == parity) . takeWhile (\(dist,pts) -> dist <= 150) $ dropWhile (\(dist,pts) -> dist < minDist) $ steps
    in sum . map (S.size . snd) $ goeieSteps

parseInput :: String -> (Grid, Point)
parseInput input =
    let ls = lines input
        w = length $ head ls
        h = length ls
        indices = map (\i -> let (y,x) = divMod i w in (x,y)) [0..]
        start = (w `div` 2, h `div` 2)
        arr = A.array ((0,0), (w-1,h-1)) $ zip indices (map (/= '#') $ concat ls)
    in (arr, start)


totaal :: Grid -> Int
totaal g =
    let k = 26501365 `div` 131
        evenvlak = traceShowId $ trace "evenvlak " $ calcAmount True 0 (g, (65,65))
        oddvlak = traceShowId $ trace "oddvlak " $ calcAmount False 0 (g, (65,65))
        evenhoeken = traceShowId $ trace "evenhoeken " $ calcAmount True 66 (g, (65,65))
        oddhoeken = traceShowId $ trace "oddhoeken " $ calcAmount False 66 (g, (65,65))
    in (k+1)*(k+1)*oddvlak -(k+1)*oddhoeken + k*k*evenvlak + k*evenhoeken
