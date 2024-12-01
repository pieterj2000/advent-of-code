import qualified Data.Array.IArray as A
import Data.Array.IArray ((!), (//))
import qualified Parser as P
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortOn, maximumBy, minimumBy, foldl', groupBy, lookup)
import Data.Ord (comparing)

import Debug.Trace
import Data.Maybe (fromJust)

inputFile = "22a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
--calc = dropBricks . sortOn lowestZ . parseInput
calc input = 
    let bricks = parseInput input
        (rustOp, draagt) = dropBricks . sortOn lowestZ $ bricks
--    in translateMap bricks . dropBricks . sortOn lowestZ $ bricks
    in length . filter id . map (canBeRemoved rustOp draagt) $ bricks

type Point3 = (Int, Int, Int)
type Point2 = (Int, Int)
type Layer = A.Array (Point2) (Int, Brick)
type Brick = (Point3, Point3)
type BrickMap = M.Map Brick (S.Set Brick)

parseInput :: String -> [Brick]
parseInput input =
    let pointP = (\a _ b _ c -> (a,b,c)) <$> P.int <*> P.char ',' <*> P.int <*> P.char ',' <*> P.int
        brickP = (,) <$> pointP <*> (P.char '~' *> pointP)
    in map (P.parseResult brickP) $ lines input

lowestZ :: Brick -> Int
lowestZ ((_,_,z1), (_,_,z2)) = min z1 z2
highestZ :: Brick -> Int
highestZ ((_,_,z1), (_,_,z2)) = max z1 z2

fst3 (a,b,c) = a
snd3 (a,b,c) = b
thd3 (a,b,c) = c

groundBrick :: [Brick] -> Brick
groundBrick bricks =
    let minX = minimum $ map fst3 $ map fst bricks ++ map snd bricks
        maxX = maximum $ map fst3 $ map fst bricks ++ map snd bricks
        minY = minimum $ map snd3 $ map fst bricks ++ map snd bricks
        maxY = maximum $ map snd3 $ map fst bricks ++ map snd bricks
    in ( (minX,minY,0), (maxX,maxY,0) )

dropBricks :: [Brick] -> (BrickMap, BrickMap)
dropBricks bricks =
    let
        grondBrick@(l,r) = groundBrick bricks
        grond = A.listArray (pxy l,pxy r) $ repeat (0,grondBrick)

        doe :: (Layer, BrickMap, BrickMap) -> Brick -> (Layer, BrickMap, BrickMap)
        doe (topLaag, rustOp, draagt) brick =
            let bp = map pxy $ brickPoints brick
                highestBricks = reverse $ sortOn fst $ map (topLaag !) bp
                highestBricks' = head $ groupBy (\a b -> fst a == fst b) highestBricks
                supportingZ = fst . head $ highestBricks'
                supportingBricks = map snd highestBricks'
                deltaZ = highestZ brick - lowestZ brick + 1
                newLaag = topLaag // [ (p, (supportingZ + deltaZ, brick)) | p<-bp ]
                newRustOp = multiMapUnion rustOp (M.singleton brick $ S.fromList supportingBricks)
                newDraagt = multiMapUnion draagt (M.fromList $ map (\b -> (b, S.singleton brick)) supportingBricks)
            in (newLaag, newRustOp, newDraagt)

    in (\(_,a,b) -> (a,b)) $ foldl' doe (grond, M.empty, M.empty) bricks

multiMapUnion :: Ord a => M.Map a (S.Set a) -> M.Map a (S.Set a) -> M.Map a (S.Set a)
multiMapUnion = M.unionWith S.union

brickPoints :: Brick -> [Point3]
brickPoints ((u,v,w),(x,y,z)) = [ (a,b,c) | a<-[u..x], b<-[v..y], c<-[w..z] ]

pxy :: Point3 -> Point2
pxy (x,y,z) = (x,y)

translateMap :: [Brick] -> BrickMap -> M.Map Char [Char]
translateMap bricks m =
    let dict = zip (groundBrick bricks : bricks) ('o':['a'..])
        f = fromJust . flip lookup dict
        m' = M.map (map f . S.toList) m
        m'' = M.mapKeys f m'
    in m''

canBeRemoved :: BrickMap -> BrickMap -> Brick -> Bool
canBeRemoved rustOp draagt brick = if brick `M.notMember` draagt then True else all doormeergedraagt (draagt M.! brick)
    where doormeergedraagt = (>1) . S.size . (rustOp M.!)