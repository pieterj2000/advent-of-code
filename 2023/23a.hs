import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import qualified Data.Set as S
import qualified Data.Map as M

inputFile = "23a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = calcMax . parseInput

type Point = (Int, Int)
type Grid = A.Array Point Char

parseInput :: String -> (Grid, Point, Point)
parseInput input =
    let ls = lines input
        w = length $ head ls
        h = length ls
        bnds = ((0,0),(w-1,h-1))
        indices = map (\i -> let (y,x) = divMod i w in (x,y)) [0..]
        arr = A.array bnds $ zip indices (concat ls)
        start = head [ (x,0) | x<-[0..(w-1)], arr ! (x,0) /= '#' ]
        end = head [ (x,h-1) | x<-[0..(w-1)], arr ! (x,h-1) /= '#' ]
    in (arr, start, end)

inGrid :: Grid -> Point -> Bool
inGrid = A.inRange . A.bounds

isPath :: Grid -> Point -> Bool
isPath g p = g ! p /= '#'

neighboursOut :: Grid -> Point -> [Point]
neighboursOut g (x,y) =  map fst .
                        filter (\(p,forbiddenChar) -> g ! p /= forbiddenChar) .
                        filter (isPath g . fst) .
                        filter (inGrid g . fst) $
                        [((x-1,y), '>'), ((x+1,y), '<'), ((x,y-1), 'v'), ((x,y+1), '^')]


makePredecessorMap :: Grid -> Point -> M.Map Point (S.Set Point)
makePredecessorMap g start =
    let doe :: Grid -> Point -> Point -> M.Map Point (S.Set Point)
        doe g previous current = M.unionWith S.union thisMap nextMaps
            where
                thisMap = M.singleton current (S.singleton previous)
                nextPts = filter (/= previous) $ neighboursOut g current
                nextMaps = M.unionsWith S.union $ map (doe g current) nextPts

        nextP = head $ neighboursOut g start
    in doe g start nextP

isAcyclic :: (Grid, Point, Point) -> Bool
isAcyclic (g, start, end) =
    let doe :: Grid -> S.Set Point -> Point -> Point -> Bool
        doe g visited previous current
            | any ( `S.member` visited) nextPts = False
            | otherwise                         = all (doe g newVisited current) nextPts
            where
                newVisited = S.insert previous visited
                nextPts = filter (/= previous) $ neighboursOut g current

        nextP = head $ neighboursOut g start
    in doe g S.empty start nextP


calcMax :: (Grid, Point, Point) -> Int
calcMax (g, start, end) =
    let distMem :: A.Array Point Int
        distMem = A.listArray (A.bounds g) [ dist p | p <- A.range (A.bounds g) ]

        predecessors = makePredecessorMap g start

        dist :: Point -> Int
        dist p
            | p == start = 0
            | otherwise = 1 + maximum (S.map (distMem !) $ predecessors M.! p)

    in distMem ! end
