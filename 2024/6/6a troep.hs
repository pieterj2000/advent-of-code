import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad (zipWithM_)
import Data.Maybe (fromJust)
import Data.STRef 


inputFile = "6a.ex.input"

main = do
    input <- readFile inputFile
    let f = parse input >>= (\(a,b,c) -> V.freeze a >>= \aa -> return (aa,b,c))
    print $ runST f

toCart :: (Int, Int) -> Int -> (Int, Int)
toCart (w,h) i = i `divMod` h
fromCart :: (Int, Int) -> (Int, Int) -> Int
fromCart (w,h) (x,y) = h*x+y

add :: Pos -> Pos -> Pos
add (x,y) (dx,dy) = (x+dx,y+dy)

rot :: Pos -> Pos
rot (x,y) = (y,-x)

type Pos = (Int, Int)
type Vec s = VM.STVector s Char
type SPos s = STRef s Pos
type State s = (Vec s, Pos, SPos s, SPos s)

parse :: String -> ST s (State s)
parse input = do
    let ls = lines input
        h = length ls
        w = length $ head ls
        size = (w,h)
    v <- VM.new (w*h)

    let doel y = zipWithM_ (\x c -> VM.write v (fromCart size (x,y)) c) [0..]
    zipWithM_ doel [0..] ls

    i <- fromJust . V.findIndex (=='^') <$> V.unsafeFreeze v
    VM.write v i 'X'
    pos <- newSTRef $ toCart size i
    dir <- newSTRef (0,-1)

    return (v,size, pos, dir)

step :: State s -> ST s (State s)
step (v, size, pos, dir) = do
    p <- readSTRef pos
    d <- readSTRef dir
    let nextpos = add p d
    
    nval <- VM.read v (fromCart size nextpos)
    case nval of
        '#' -> do
            ndir = 


    return (v,size,pos,dir)

calc :: State s -> ST s Int
calc reports = undefined

