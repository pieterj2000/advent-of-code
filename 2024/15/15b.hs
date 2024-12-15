import qualified Data.IntMap as M
import qualified Data.Set as S
import qualified Data.Ix as Ix
import Linear.V2
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (join)
import Data.List (foldl')


inputFile = "15a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input



type Pos = V2 Int
type Grid = (M.IntMap Int, Pos, S.Set Pos, (V2 Int, V2 Int))


parse :: String -> (Grid, [Pos])
parse input =
    let (ls, sp) = break null $ lines input
        pm '^' = V2 0 (-1)
        pm 'v' = V2 0 1
        pm '>' = V2 1 0
        pm '<' = V2 (-1) 0
        dirs = map pm $ concat sp

        h = length ls
        w = length $ head ls
        r = (V2 1 1,V2 (2*w) h)

        pp '.' _ = 0
        pp '#' _ = 1
        pp 'O' True = 2
        pp 'O' False = 3
        pp '@' _ = 4
        doel y = M.fromList . concat . zipWith (\x c -> if c /= '@'
                then [(Ix.index r (V2 x y), pp c True), (Ix.index r (V2 (x+1) y), pp c False)]
                else [(Ix.index r (V2 x y), pp c True), (Ix.index r (V2 (x+1) y), 0)]) [1,3..]
        intmap = M.unions $ zipWith doel [1..] ls

        kerstmanpos = head $ filter ((==4) . (intmap M.!) . Ix.index r) $ Ix.range r
        boxes = S.fromList $ filter ((==2) . (intmap M.!) . Ix.index r) $ Ix.range r
    in ((intmap,kerstmanpos, boxes, r), dirs)


isruimte' :: Grid -> Pos -> Bool
isruimte' (m, _,_,r) p = Ix.inRange r p && (m M.! Ix.index r p) == 0

isruimte :: Grid -> [Pos] -> Bool
isruimte g = all (isruimte' g)

trymove :: Grid -> Pos -> Maybe Grid
trymove g@(_, k, _, _) d = case trymoveint g [k] d of
    Nothing -> Nothing
    Just (a,b,c,r) -> Just (a, b + d, c, r)

insertListMap :: (V2 Int, V2 Int) -> [(Pos, Int)] -> M.IntMap Int -> M.IntMap Int
insertListMap r [] m = m
insertListMap r ((p,v):xs) m = insertListMap r xs (M.insert (Ix.index r p) v m)

insertListSet :: [Pos] -> S.Set Pos -> S.Set Pos
insertListSet xs s = foldl' (flip S.insert) s xs

deleteListSet :: [Pos] -> S.Set Pos -> S.Set Pos
deleteListSet xs s = foldl' (flip S.delete) s xs

trymoveint :: Grid -> [Pos] -> Pos -> Maybe Grid
trymoveint g@(intmap,_,_,r) p d
    | anyiswall     = Nothing
    | clear         = Just g''
    | otherwise     = case gm' of
        Nothing -> Nothing
        Just _  -> Just g''
    where
        getcur q = intmap M.! Ix.index r q
        clear = isruimte g p'
        next q
            | getcur (q+d) == 0 = []
            | getcur (q+d) == 2 && (let (V2 dx dy) = d in dy /= 0) = [q + d, q + (V2 1 0) + d]
            | getcur (q+d) == 3 && (let (V2 dx dy) = d in dy /= 0) = [q + d, q + (V2 (-1) 0) + d]
            | otherwise = [q + d]
        p' = concatMap next p
        next'' q = [(q + d, getcur q)]
        p'' = concatMap next'' p
        anyiswall = any ((==1) . getcur) p
        iskerstman = length p == 1 && getcur (head p) == 4
        gm' = trymoveint g p' d
        (intmap2, kp, boxes, r2) = if clear then g else fromMaybe g gm'
        newmap = insertListMap r2 p'' $ insertListMap r2 (map (\q -> (q,0)) p) intmap2
        newboxes = if iskerstman then insertListSet p' $ deleteListSet p boxes else boxes
        g'' = (newmap, kp, newboxes, r2)

dostep :: Grid -> Pos -> Grid
dostep g p = fromMaybe g (trymove g p)

dosteps :: Grid -> [Pos] -> Grid
dosteps = foldl' dostep

dosteps' :: Grid -> [Pos] -> [Pos]
dosteps' g@(_,k,_,_) [] = [k]
dosteps' g@(_,k,_,_) (p:ps) = k : dosteps' (dostep g p) ps

dosteps'' :: Grid -> [Pos] -> String
dosteps'' g@(_,k,_,_) [] = printg g
dosteps'' g@(_,k,_,_) (p:ps) = printg g ++ "\n\nmove: " ++ [pm p] ++ "\n\n" ++ dosteps'' (dostep g p) ps
    where
        pm (V2 0 (-1)) = '^'
        pm (V2 0 1) = 'v'
        pm (V2 1 0) = '>'
        pm (V2 (-1) 0) = '<'


gps :: Pos -> Int
gps (V2 x y) = (x-1)+100*(y-1)

gpsg :: Grid -> Int
gpsg g = sum $ map gps $ getboxes g

getboxes :: Grid -> [Pos]
getboxes (m,_,_,r) = filter ((==2) . (m M.!) . Ix.index r) $ Ix.range r

--calc :: (Grid, [Pos]) -> Int
calc (g,ps) = gpsg $ dosteps g ps

printg :: Grid -> String
printg (m,_,_,r@(_,V2 w h)) = unlines $ map doel [1..h]
    where
        doel y = map (\x -> pp $ m M.! (Ix.index r (V2 x y))) [1..w]
        pp 0 = '.'
        pp 1 = '#'
        pp 2 = '['
        pp 3 = ']'
        pp 4 = '@'
