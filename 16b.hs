import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

inputFile = "16a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc input =    let g = parseInput input
                    options = map (flip withInitialState g) (makeInitialStates g)
                in maximum options

makeInitialStates :: Grid -> [S]
makeInitialStates g =   let w = width g
                            h = height g
                        in  [ S M.empty (0,y) R | y<-[0..(h-1)] ] ++
                            [ S M.empty (w-1,y) L | y<-[0..(h-1)] ] ++
                            [ S M.empty (x,0) D | x<-[0..(w-1)] ] ++
                            [ S M.empty (x,h-1) U | x<-[0..(w-1)] ]

withInitialState :: S -> Grid -> Int
withInitialState state = M.size . runReader (evalStateT step state)

parseInput :: String -> Grid
parseInput input = Grid (V.fromList $ filter (/='\n') input) (length $ head $ lines input) (length $ lines input)

printVGrid :: VGrid -> IO ()
printVGrid g = mapM_ (\y -> 
    mapM_ (\x -> 
        putStr (
            let (u,d,l,r) = M.findWithDefault (False, False, False, False) (x,y) g 
            in if or [u,d,l,r] then "#" else ".")) [0..9] >> putStrLn "") [0..9]

type Point = (Int, Int)
type M a = StateT S (Reader Grid) a
data Grid = Grid { grid :: V.Vector Char, width :: Int, height :: Int } deriving (Show, Read)
data Dir = U | D | L | R deriving (Show, Read, Eq)
type VGrid = M.Map Point (Bool, Bool, Bool, Bool) -- (U,D,L,R)
data S = S { visited :: VGrid, loc :: Point, dir :: Dir }

inBounds :: Point -> M Bool
inBounds (x,y) = do
    w <- asks width
    h <- asks height
    return (x >= 0 && x < w && y >= 0 && y < h)

moveP :: Point -> Dir -> Point
moveP (x,y) U = (x,y-1)
moveP (x,y) D = (x,y+1)
moveP (x,y) L = (x-1,y)
moveP (x,y) R = (x+1,y)

getSpace :: M Char
getSpace = do
    (x,y) <- gets loc
    grid <- asks grid
    w <- asks width
    return $ grid V.! (x+w*y)

getVisited :: M Bool
getVisited = do
    p <- gets loc
    map <- gets visited
    dir <- gets dir
    let (u,d,l,r) = M.findWithDefault (False, False, False, False) p map
        geweest = case dir of
            U -> u
            D -> d
            L -> l
            R -> r
    return geweest

setVisited :: M ()
setVisited = do
    p <- gets loc
    map <- gets visited
    dir <- gets dir
    let (u,d,l,r) = M.findWithDefault (False, False, False, False) p map
        nieuw = case dir of
            U -> (True,d,l,r)
            D -> (u,True,l,r)
            L -> (u,d,True,r)
            R -> (u,d,l,True)
    modify $ \s -> s { visited = M.insert p nieuw map}

finish :: M VGrid
finish = gets visited

move :: M VGrid
move = do
    p <- gets loc
    dir <- gets dir
    let nextp = moveP p dir
    inbounds <- inBounds nextp
    if inbounds
        then modify (\s -> s { loc = nextp }) >> step
        else finish

mirror :: Dir -> Dir
mirror U = R
mirror D = L
mirror L = D
mirror R = U

mirror2 :: Dir -> Dir
mirror2 U = L
mirror2 D = R
mirror2 L = U
mirror2 R = D

splitter :: Dir -> (Dir, Dir)
splitter U = (L, R)
splitter D = (L, R)
splitter L = (U, D)
splitter R = (U, D)

setDir :: Dir -> M ()
setDir d = modify $ \s -> s { dir = d }

split :: M VGrid
split = do
    (d1,d2) <- gets (splitter . dir)
    curState <- get
    result1 <- setDir d1 >> move
    put $ curState { visited = result1 }
    setDir d2
    move


step :: M VGrid
step = do
    v <- getVisited
    
    if v then finish
    else do
        setVisited
        c <- getSpace
        dir <- gets dir

        case c of
            '.' -> move
            '/' -> setDir (mirror dir) >> move
            '\\' -> setDir (mirror2 dir) >> move
            '-' -> if dir == U || dir == D then split else move
            '|' -> if dir == L || dir == R then split else move

