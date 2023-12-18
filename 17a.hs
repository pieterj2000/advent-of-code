import qualified Data.Heap as H
import qualified Data.Array.IArray as A
import qualified Data.HashMap.Lazy as M
import Data.Char (digitToInt, intToDigit)


import Debug.Trace
import Data.Hashable (Hashable (hashWithSalt))

inputFile = "17a.input"

main = do
    input <- readFile inputFile
    print $ calc input

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

type Point = (Int, Int)
data Dir = U | D | L | R deriving (Show, Eq, Enum)
data Node = Node { loc :: Point, dir :: Dir, straight :: Int } deriving (Show, Eq)
type Grid = A.Array Point Int

instance Ord Node where
    compare _ _ = EQ
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

neighbours :: Grid -> Node -> [Node]
neighbours g (Node p dir straight) = nodes
    where   possDirs = filter (\d -> not (dir == d && straight >= 3) && not (opposites d dir)) [U,D,L,R]
            possPoints = map (\d -> (move p d,d)) possDirs
            validPoints = filter (A.inRange (A.bounds g) . fst) possPoints
            nodes = map (\(q,d) -> Node q d (if d == dir then straight + 1 else 1)) validPoints


search :: Grid -> Node -> Point -> Int
search graph start dest = step graph dest (H.singleton (start, 0)) initialDists
    where
        ps = A.range $ A.bounds graph
        initialDists = M.fromList [ (Node p dir s, (Node (0,0) R 0, maxBound :: Int)) | p<-ps, dir<-[U .. R], s<-[0..4] ]

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


step :: Grid -> Point -> H.Heap (Node, Int) -> M.HashMap Node (Node, Int) -> Int
step graph dest queue dists | H.null queue                  = error $ show dists
                            | loc q == dest                 = qDist             -- einde gevonden
                            | (snd $ dists M.! q) < qDist   = step graph dest (H.deleteMin queue) dists -- is al een betere weg gevonden naar node, deze is oud
                            | otherwise                     = step graph dest newQueue newDists
            where
                (q,qDist) = H.minimum queue
                neighb = neighbours graph q
                neighbD = map (\n -> (n, qDist + (graph A.! loc n))) neighb
                newQueue = H.union (H.deleteMin queue) (H.fromList neighbD)
                mergef n (p1,d1) (p2,d2) = if d1 < d2 then (p1,d1) else (p2,d2)
                newDists = M.unionWithKey mergef dists $ M.fromList $ map (\(n,d) -> (n, (q,d))) neighbD



{-


search :: Grid -> Node -> Point -> Int
search graph start dest = step graph dest (H.singleton (start, 0)) (A.listArray (A.bounds graph) [ maxBound :: Int | x<-A.range $ A.bounds graph ] )


doprint :: Grid -> String
doprint g = let (_,(w,h)) = A.bounds g
            in concat $ map (\y -> map (\x -> intToDigit $ (\x -> if x > 10 then 0 else x) $ g A.! (x,y)) [0..(w-1)] ++ "\n") [0..(h-1)]


step :: Grid -> Point -> H.Heap (Node, Int) -> Grid -> Int
step graph dest queue dists | H.null queue                  = error $ show dists
                            | loc q == dest                 = trace (doprint dists) $ qDist             -- einde gevonden
                            | (dists A.! loc q) < qDist     = step graph dest (H.deleteMin queue) dists -- is al een betere weg gevonden naar node, deze is oud
                            | otherwise                     = traceShow q $ step graph dest newQueue newDists
            where
                (q,qDist) = H.minimum queue
                neighb = neighbours graph q
                neighbD = map (\n -> (n, qDist + (graph A.! loc n))) neighb
                newQueue = H.union (H.deleteMin queue) (H.fromList neighbD)
                newDists = A.accum min dists $ map (\(n,d) -> (loc n, d)) neighbD




-}