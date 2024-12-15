import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Ix as Ix
import Linear.V2
import Control.Monad (zipWithM_)
import Data.Char (digitToInt)
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Maybe (mapMaybe, fromJust)
import Data.List (foldl')


inputFile = "10a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input


type Pos = V2 Int
type Grid = (Pos -> Maybe Int, (V2 Int, V2 Int))

parse :: String -> Grid
parse input = runST $ do
    let ls = lines input
        h = length ls
        w = length $ head ls
        r = (V2 1 1,V2 w h)
    v <- VM.new (Ix.rangeSize r)

    let doel y = zipWithM_ (\x c -> VM.write v (Ix.index r (V2 x y)) (digitToInt c)) [1..]
    zipWithM_ doel [1..] ls

    v' <- V.freeze v

    let get :: Pos -> Maybe Int
        get p = if inbound then Just $ v' V.! Ix.index r p else Nothing
            where
                inbound = Ix.inRange r p

    return (get, r)

getzeros :: Grid -> [Pos]
getzeros (get, r) = filter ((== Just 0) . get) $ Ix.range r

reachable :: Grid -> Pos -> [Pos]
reachable (get,r) s = doe (Set.singleton $ Ix.index r s) $ Seq.singleton s
    where
        doe :: Set.IntSet -> Seq.Seq Pos -> [Pos]
        doe gehad Seq.Empty = []
        doe gehad (p Seq.:<| ps) = p : doe gehad' (ps Seq.>< Seq.fromList newps)
            where
                dirs = take 4 $ iterate perp (V2 1 0)
                h = fromJust $ get p
                newps = filter (\q -> (Ix.index r q) `Set.notMember` gehad) $ filter ((== Just (h+1)) . get)  $ map (p +) dirs
                gehad' = foldl' (flip Set.insert) gehad $ map (Ix.index r) newps

numnines :: Grid -> Pos -> Int
numnines g@(get,_) p = length $ filter (\q -> get q == Just 9) $ reachable g p


calc :: Grid -> Int
calc g = sum $ map (numnines g) $ getzeros g

