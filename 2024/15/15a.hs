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
        r = (V2 1 1,V2 w h)

        pp '.' = 0
        pp '#' = 1
        pp 'O' = 2
        pp '@' = 3
        doel y = M.fromList . zipWith (\x c -> (Ix.index r (V2 x y), pp c)) [1..]
        intmap = M.unions $ zipWith doel [1..] ls

        kerstmanpos = head $ filter ((==3) . (intmap M.!) . Ix.index r) $ Ix.range r
        boxes = S.fromList $ filter ((==2) . (intmap M.!) . Ix.index r) $ Ix.range r
    in ((intmap,kerstmanpos, boxes, r), dirs)


isruimte :: Grid -> Pos -> Bool
isruimte (m, _,_,r) p = Ix.inRange r p && (m M.! Ix.index r p) == 0

trymove :: Grid -> Pos -> Maybe Grid
trymove g@(_, k, _, _) d = case trymoveint g k d of
    Nothing -> Nothing
    Just (a,b,c,r) -> Just (a, b + d, c, r)

trymoveint :: Grid -> Pos -> Pos -> Maybe Grid
trymoveint g@(intmap,_,_,r) p d
    | cur == 1      = Nothing
    | clear         = Just g''
    | otherwise     = case gm' of
        Nothing -> Nothing
        Just _  -> Just g''
    where
        clear = isruimte g p'
        p' = p + d
        cur = intmap M.! Ix.index r p
        gm' = trymoveint g (p+d) d
        (intmap2, kp, boxes, r2) = if clear then g else fromMaybe g gm'
        newmap = M.insert (Ix.index r2 p') cur $ M.insert (Ix.index r2 p) 0 intmap2
        newboxes = if cur == 2 then S.insert p' $ S.delete p boxes else boxes
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
gpsg (_,_,boxes,_) = sum $ S.map gps boxes

--calc :: (Grid, [Pos]) -> Int
calc (g,ps) = gpsg $ dosteps g ps

printg :: Grid -> String
printg (m,_,_,r@(_,V2 w h)) = unlines $ map doel [1..h]
    where
        doel y = map (\x -> pp $ m M.! (Ix.index r (V2 x y))) [1..w]
        pp 0 = '.'
        pp 1 = '#'
        pp 2 = 'O'
        pp 3 = '@'
