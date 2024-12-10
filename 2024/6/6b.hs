import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad (zipWithM_)
import Data.Maybe (fromJust)
import Linear.V2
import Data.List (unfoldr)
import Data.Containers.ListUtils (nubOrd)

inputFile = "6a.input"

main = do
    input <- readFile inputFile
    print $ calc . parse $ input
    --print $ (\((v,s),_,_) -> (V.length v, s)) . parse $ input

toCart :: (Int, Int) -> Int -> Pos
toCart (w,h) i = V2 (i `div` h) (i `mod` h)
fromCart :: (Int, Int) -> Pos -> Int
fromCart (w,h) (V2 x y) = h*x+y


type Pos = V2 Int
type Grid = (V.Vector Char, (Int, Int))

(!) :: Grid -> Pos -> Maybe Char
(v, size) ! i = if inbound then v V.!? fromCart size i else Nothing
    where
        (V2 x y) = i
        (w,h) = size
        inbound = x >= 0 && y >= 0 && x < w && y < h

parse :: String -> (Grid, Pos, Pos)
parse input = runST $ do
    let ls = lines input
        h = length ls
        w = length $ head ls
        size = (w,h)
    v <- VM.new (w*h)

    let doel y = zipWithM_ (\x c -> VM.write v (fromCart size (V2 x y)) c) [0..]
    zipWithM_ doel [0..] ls

    i <- fromJust . V.findIndex (=='^') <$> V.unsafeFreeze v
    VM.write v i '.'
    let pos = toCart size i
        dir = V2 0 (-1)
    v' <- V.freeze v
    return ((v',size), pos, dir)


getPath :: (Grid, Pos, Pos) -> [Pos]
getPath (grid, initpos, initdir) =
    let doe (p, d) = stap <$> nextpos
            where
                stap c
                    | c == '#' && altpos == Just '#' = (p-d,(p-d, -d))
                    | c == '#' = (p+d', (p+d', d'))
                    | otherwise = (p+d,(p+d, d))
                d' = perp d
                nextpos = grid ! (p + d)
                altpos = grid ! (p + d')

    in initpos : unfoldr doe (initpos, initdir)

calc :: (Grid, Pos, Pos) -> Int
calc spul = 
    let spots = nubOrd $ getPath spul
        goodspots = filter (loopWithBlockAt spul) spots
    in length goodspots

hasLoop :: [Pos] -> Bool
hasLoop ls = doe (drop 10000 ls) (drop 10000 ls)
    where
        doe [] _ = False
        doe _ [] = False
        doe _ [x] = False
        doe [x] _ = False
        doe (x:xs) (_:y:ys)
            | x == y && (head xs == head ys)= True
            | otherwise = doe xs ys


loopWithBlockAt :: (Grid, Pos, Pos) -> Pos -> Bool
loopWithBlockAt ((v,size), initpos, initdir) p =
    let v' = (V.//) v [(fromCart size p, '#')]
        path = getPath ((v', size),initpos, initdir)
    in hasLoop path


