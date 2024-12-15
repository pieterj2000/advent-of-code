import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Ix as Ix
import Linear.V2
import Control.Monad (zipWithM_)
import qualified Data.Sequence as Seq
import qualified Data.IntSet as Set
import Data.Maybe (mapMaybe, fromJust)
import Data.List (foldl')
import Data.Char (ord)


inputFile = "12a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input


type Pos = V2 Int
type Grid = (Pos -> Pos -> Bool, (V2 Int, V2 Int))

parse :: String -> Grid
parse input = runST $ do
    let ls = lines input
        h = length ls
        w = length $ head ls
        r = (V2 1 1,V2 w h)
    v <- VM.new (Ix.rangeSize r)

    let doel y = zipWithM_ (\x c -> VM.write v (Ix.index r (V2 x y)) (ord c)) [1..]
    zipWithM_ doel [1..] ls

    v' <- V.freeze v

    let get :: Pos -> Pos -> Bool
        get p q = inbound && ((v' V.! Ix.index r p) == (v' V.! Ix.index r q))
            where
                inbound = Ix.inRange r p && Ix.inRange r q

    return (get, r)


reachable :: Grid -> Pos -> (Set.IntSet, Int)
reachable (get,r) s = doe 0 Set.empty $ Seq.singleton s
    where
        doe :: Int -> Set.IntSet -> Seq.Seq Pos -> (Set.IntSet, Int)
        doe t gehad Seq.Empty = (gehad,t)
        doe per gehad (p Seq.:<| ps)
            | (Ix.index r p) `Set.member` gehad = doe per gehad ps
            | otherwise                         = doe (per+newfences) gehad' (ps Seq.>< Seq.fromList samefieldneighs)
            where
                dirs = take 4 $ iterate perp (V2 1 0)
                samefieldneighs = filter (get p)  $ map (p +) dirs
                newfences = 4 - length samefieldneighs
                gehad' = Set.insert (Ix.index r p) gehad


calcfields :: Grid -> [Pos] -> Int
calcfields g [] = 0
calcfields g (p:ps) = (Set.size deel * per) + calcfields g ps'
    where
        (deel,per) = reachable g p
        ps' = filter (\q -> Ix.index (snd g) q `Set.notMember` deel) ps
        


--calc :: Grid -> Int
calc g = calcfields g $ Ix.range (snd g)
