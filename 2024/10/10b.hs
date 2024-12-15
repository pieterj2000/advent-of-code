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

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

reachable :: Grid -> Pos -> [[Pos]]
reachable (get,r) = doe . Seq.singleton . (: [])
    where
        doe :: Seq.Seq [Pos] -> [[Pos]]
        doe Seq.Empty = []
        doe ((p:ps) Seq.:<| rest) = finished ++ doe (rest Seq.>< Seq.fromList newps)
            where
                dirs = take 4 $ iterate perp (V2 1 0)
                h = fromJust $ get p
                neighs = filter ((== Just (h+1)) . get)  $ map (p +) dirs
                newps = map (:p:ps) neighs
                finished = filter ((== Just 9) . get . head) newps



calc :: Grid -> Int
calc g = sum . map (length . reachable g) $ getzeros g

