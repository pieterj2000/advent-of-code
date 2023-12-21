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
data Node = Node { loc :: Point, dir :: Dir, straight :: Int } deriving (Show, Eq, Ord)
type Grid = A.UArray Point Int

--instance Ord Node where
--    compare _ _ = EQ
-- Tuple (Node, Int) heeft ordering lexicographisch, dus als Node altijd gelijk is, pakt hij (zoals gewenst) vergelijking van de ints

instance Hashable Node where
    hashWithSalt s (Node loc dir straight) = s `hashWithSalt` loc `hashWithSalt` (fromEnum dir) `hashWithSalt` straight


move :: Point -> Dir -> Point
move (x,y) U = (x,y-1)
move (x,y) D = (x,y+1)
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)

opposites :: Dir -> Dir -> Bool
opposites U D = True
opposites D U = True
opposites L R = True
opposites R L = True
opposites _ _ = False

--calc :: String -> Int
calc input =    let grid = parseInput input
                    end = snd $ A.bounds grid
                    start = Node (0,0) R 0
                in search grid start end

parseInput :: String -> Grid
parseInput input =  let w = length $ head $ lines input
                        h = length $ lines input
                        indices = map (\i -> let (y,x) = divMod i w in (x,y)) [0..]
                    in A.array ((0,0), (w-1,h-1)) $ zip indices $ map digitToInt $ filter (/='\n') input


neighbours :: Grid -> Node -> [Node]
neighbours g (Node p dir straight) = nodes
    where   possDirs = filter (\d -> not (dir == d && straight >= 3) && not (opposites d dir)) [U,D,L,R]
            possPoints = map (\d -> (move p d,d)) possDirs
            validPoints = filter (A.inRange (A.bounds g) . fst) possPoints
            nodes = map (\(q,d) -> Node q d (if d == dir then straight + 1 else 1)) validPoints


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

        insert :: (Q.HashPSQ Node Int Int, HM.HashMap Node Int) -> Node -> (Q.HashPSQ Node Int Int, HM.HashMap Node Int)
        insert (queue, dists) node = 
            let dist = qDist + (graph A.! loc node)
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
