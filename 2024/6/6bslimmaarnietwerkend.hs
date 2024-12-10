import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad (zipWithM_)
import Data.Maybe (fromJust, catMaybes, isJust)
import Linear.V2
import Data.List (unfoldr, (\\), zipWith4)
import Data.Containers.ListUtils (nubOrd)
import Linear (Metric(quadrance))
import Data.List (transpose)
import qualified Data.Set as S

inputFile = "6a.input"

main = do
    input <- readFile inputFile
    let (grid, startpos, startdir) = parse $ input
        path = getPath (grid, startpos, startdir)
        pgrid = maakPathGrid grid path
        boxes = getBlocks path
    print $ length $ nubOrd $ getNewBoxes pgrid S.empty boxes
    return ()

toCart :: (Int, Int) -> Int -> Pos
toCart (w,h) i = V2 (i `div` h) (i `mod` h)
fromCart :: (Int, Int) -> Pos -> Int
fromCart (w,h) (V2 x y) = h*x+y


type Pos = V2 Int
type Grid = (V.Vector Char, (Int, Int))
type Path = [(Pos, Pos)]
type PathGrid = (V.Vector (Int, Pos), (Int, Int))

(!) :: Grid -> Pos -> Maybe Char
(v, size) ! i = if inbound then v V.!? fromCart size i else Nothing
    where
        (V2 x y) = i
        (w,h) = size
        inbound = x >= 0 && y >= 0 && x < w && y < h

(.!) :: PathGrid -> Pos -> Maybe (Int, Pos)
(v, size) .! i = if inbound then v V.!? fromCart size i else Nothing
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


rotrechts :: Pos -> Pos
rotrechts = perp

getPath :: (Grid, Pos, Pos) -> Path
getPath (grid, initpos, initdir) =
    let doe (p, d)= stap <$> nextpos
            where
                stap c
                    | c == '#' = let d' = rotrechts d in ((p+d', d'), (p+d', d'))
                    | otherwise = ((p+d, d),(p+d, d))
                nextpos = grid ! (p + d)
        (poss, dirs) = unzip $ (initpos, initdir) : unfoldr doe (initpos, initdir)

        repeatlast :: [a] -> [a]
        repeatlast [x] = repeat x
        repeatlast (x:xs) = x : repeatlast xs

        path = zip poss $ tail $ repeatlast dirs
    in path

--calc :: (Grid, Pos, Pos) -> Int
--calc = length . nubOrd . getPath
calc = getPath
--calc s = let p = getPath s; u = nubOrd p in p \\ u


getBlocks :: Path -> [(Pos, Pos, Int)]
getBlocks ls' = catMaybes $ zipWith4 doe ls (tail ls) (drop 2 ls) [2..] -- kan geupdate worden met dat dir nu in path zit evt
    where
        ls = map fst ls'
        doe :: Pos -> Pos -> Pos -> Int -> Maybe (Pos, Pos, Int)
        doe prev cur next curtime
            | quadrance (next - prev) == 4 = Nothing -- no turning
            | otherwise = Just (doos, dir, curtime)
            where
                dir = cur - prev
                doos = cur + dir



maakPathGrid :: Grid -> Path -> PathGrid
maakPathGrid (grid, size) path = runST $ do
    let (w,h) = size

    pgrid <- V.thaw $ V.map (\c -> if c == '#' then (-2, V2 0 0) else (0, V2 0 0)) grid

    let mf new oud
            | fst oud == 0 = new
            | fst oud == -2 = oud
            | otherwise = if fst oud < fst new then oud else new
    zipWithM_ (\(p, dir) v -> VM.modify pgrid (mf (v,dir)) (fromCart size p)) path [1..]

    pgrid' <- V.unsafeFreeze pgrid

    return (pgrid', size)


printPathGrid :: Grid -> PathGrid -> IO ()
printPathGrid (_,(w,h)) pg = mapM_ (putStrLn . concat) $ transpose $ doe $ map (toc . fst) $ V.toList $ fst pg
    where
        toc 0 = "   "
        toc (-2) = " ##"
        toc (-1) = " xx"
        toc n = reverse $ take 3 $ reverse (show n) ++ repeat ' '

        doe [] = []
        doe xs = take w xs : doe (drop w xs)



asdf = do
    input <- readFile inputFile
    let (grid, startpos, startdir) = parse $ input
        path = getPath (grid, startpos, startdir)
        pgrid = maakPathGrid grid path
        boxes = getBlocks path
    print boxes
    printPathGrid grid pgrid
    print $ concat $ map fst $ map (handleBox pgrid) boxes
    print $ length $ concat $ map fst $ map (handleBox pgrid) boxes
    print $ length $ nubOrd $ concat $ map fst $ map (handleBox pgrid) boxes
    print $ nubOrd $ getNewBoxes pgrid S.empty boxes
    print $ length $ getNewBoxes pgrid S.empty boxes
    print $ length $ nubOrd $ getNewBoxes pgrid S.empty boxes
    return ()
    --print $ (\((v,s),_,_) -> (V.length v, s)) . parse $ input



handleBox :: PathGrid -> (Pos, Pos, Int) -> ([Pos], [(Pos, Pos, Int)])
handleBox pgrid'@(pgrid, size) (box, incomingdir, impacttime) =
    let (w,h) = size
        raydir = -incomingdir
        rightdir = rotrechts raydir
        leftdir = -rightdir
        inbound (V2 x y) = x >= 0 && y >= 0 && x < w && y < h
        ray = takeWhile (\p -> inbound p && (fst <$> pgrid' .! p) /= Just (-2)) $ tail $ iterate (+raydir) box

        canHaveBox :: Pos -> Bool
        canHaveBox p = boxinbound && geensteenrechts && lijnlinks && lijnincoming && intersectiontoekomst
            where
                links = p + leftdir
                rechts = p + rightdir
                leftinbound = isJust $ pgrid' .! links
                leftval = fromJust $ pgrid' .! links
                rightinbound = isJust $ pgrid' .! rechts
                rightval = fromJust $ pgrid' .! rechts
                pval = fromJust $ pgrid' .! p

                boxinbound = rightinbound
                geensteenrechts = fst rightval /= -2

                lijnlinks = leftinbound && (fst leftval >= 1 || fst leftval == -1)
                lijnincoming = snd leftval == rightdir
                intersectiontoekomst = fst rightval > impacttime


        boxposities = map (+rightdir) $ filter canHaveBox ray

        canHaveRay :: Pos -> Bool
        canHaveRay p = boxinbound && steenrechts && linksinbound
            where
                links = p + leftdir
                rechts = p + rightdir
                rightinbound = isJust $ pgrid' .! rechts
                rightval = fromJust $ pgrid' .! rechts

                boxinbound = rightinbound
                steenrechts = fst rightval == -2
                linksinbound = isJust $ pgrid' .! links

        newrays = filter canHaveRay ray
        newrays' = map (\p -> (p, rightdir, impacttime)) newrays
        -- check ook divergent paths zeg maar


    in (boxposities, newrays')


getNewBoxes :: PathGrid -> S.Set (Pos, Pos, Int) -> [(Pos, Pos, Int)] -> [Pos]
getNewBoxes _ set [] = []
getNewBoxes pgrid set (b:bs)
    | S.member b set = getNewBoxes pgrid set bs
    | otherwise = posboxes ++ getNewBoxes pgrid set' (newrays ++ bs)
    where
        (posboxes, newrays) = handleBox pgrid b
        set' = S.insert b set