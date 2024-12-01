import qualified Data.Array.Unboxed as A
import qualified Data.HashPSQ as Q
import Data.Char (digitToInt, intToDigit)


import Debug.Trace
import Data.Hashable (Hashable (hashWithSalt))
import Data.Maybe (fromJust)
import Data.List (foldl1', foldl')
import qualified Data.HashMap.Lazy as HM

inputFile = "17a.input"

main = do
    input <- readFile inputFile
    print $ calc input


type Point = (Int, Int)
data Dir = U | D | L | R deriving (Show, Eq, Enum, Ord)
data Node = Node { loc :: Point, dir :: Dir } deriving (Show, Eq, Ord)
type Grid = A.UArray Point Int

--instance Ord Node where
--    compare _ _ = EQ
-- Tuple (Node, Int) heeft ordering lexicographisch, dus als Node altijd gelijk is, pakt hij (zoals gewenst) vergelijking van de ints

instance Hashable Node where
    hashWithSalt s (Node loc dir) = s `hashWithSalt` loc `hashWithSalt` (fromEnum dir)


move :: Point -> Dir -> Point
move (x,y) U = (x,y-1)
move (x,y) D = (x,y+1)
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)

moveSeq :: Point -> [Dir] -> Point
moveSeq p [] = p
moveSeq p (d:dr) = moveSeq (move p d) dr

moveStraightThenDir :: Point -> Dir -> Int -> Dir -> [Point]
moveStraightThenDir p _ 0 finalDir = [move p finalDir]
moveStraightThenDir p straightDir straightDist finalDir = pNew : moveStraightThenDir pNew straightDir (straightDist - 1) finalDir
    where pNew = move p straightDir

opposites :: Dir -> Dir -> Bool
opposites U D = True
opposites D U = True
opposites L R = True
opposites R L = True
opposites _ _ = False

--calc :: String -> Int
calc input =    let grid = parseInput input
                    end = snd $ A.bounds grid
                    start = Node (0,0) R
                in search grid start end

parseInput :: String -> Grid
parseInput input =  let w = length $ head $ lines input
                        h = length $ lines input
                        indices = map (\i -> let (y,x) = divMod i w in (x,y)) [0..]
                    in A.array ((0,0), (w-1,h-1)) $ zip indices $ map digitToInt $ filter (/='\n') input

getPathWeight :: Grid -> [Point] -> Int
getPathWeight _ [] = 0
getPathWeight g (p:ps) = (g A.! p) + getPathWeight g ps

neighbours :: Grid -> Node -> [(Node, Int)]
neighbours g (Node p dir) = nodes
    where   possDirs = filter (\d -> (dir /= d) && not (opposites d dir)) [U,D,L,R]
            possPaths = [ (moveStraightThenDir p dir straight d, d) | straight<-[0..2], d<-possDirs]
            possPoints = map (\(path, finalDir) -> (last path, finalDir, getPathWeight g path)) possPaths
            validPoints = filter (\(point, dir, weight) -> A.inRange (A.bounds g) point) possPoints
            nodes = map (\(point, dir, weight) -> (Node point dir, weight)) validPoints


search :: Grid -> Node -> Point -> Int
search graph start dest = step graph dest (Q.singleton start 0 0) (HM.singleton start 0)


step :: Grid -> Point -> Q.HashPSQ Node Int Int -> HM.HashMap Node Int -> Int
step graph dest queue dists
    | Q.null queue          = error $ show queue
    | loc q == dest         = qDist             -- einde gevonden
    | otherwise             = step graph dest newQueue newDists
    where
        (q,qDist,_) = fromJust $ Q.findMin queue
        neighb = neighbours graph q

        insert :: (Q.HashPSQ Node Int Int, HM.HashMap Node Int) -> (Node, Int) -> (Q.HashPSQ Node Int Int, HM.HashMap Node Int)
        insert (queue, dists) (node, weight) =
            let dist = qDist + weight
            in case HM.lookup node dists of
                Nothing -> (Q.insert node dist 0 queue, HM.insert node dist dists)
                Just d
                    | d < dist  -> (queue, dists)
                    | otherwise -> (Q.insert node dist 0 queue, HM.insert node dist dists)

        (newQueue, newDists) = foldl insert (Q.deleteMin queue, dists) neighb




showDir U = '^'
showDir D = 'v'
showDir L = '<'
showDir R = '>'

showSpec :: Point -> Point -> Char
showSpec (xprev, yprev) (xnew,ynew) | xprev < xnew = '<'
                                    | xprev > xnew = '>'
                                    | yprev > ynew = 'v'
                                    | yprev < ynew = '^'
                                    | otherwise = 'o'

doprint :: A.Array Point (Node, Int) -> String
doprint g = let (_,(w,h)) = A.bounds g
            in --show $ g A.! (12,9)
               concat $ map
                (\y -> map
                --(\x -> (\(n,d) -> if straight n < 0 then '.' else showDir (dir n)) $ g A.! (x,y)) [0..w] ++ "\n") [0..h]
                (\x -> (\(n,d) -> showSpec (loc n) (x,y) ) $ g A.! (x,y)) [0..w] ++ "\n") [0..h]
                --(\x -> (\(n,d) -> intToDigit (straight n)) $ g A.! (x,y)) [0..w] ++ "\n") [0..h]
