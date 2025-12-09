

import Data.List (maximumBy, sort, scanl')
import Data.Ord (comparing)


inputFile = "9a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [(Int, Int)]
parse = map ((\[a,b] -> (a,b)) . take 2 . map read . words . map (\c -> if c==',' then ' ' else c)) . lines

--calc :: [[Int]] -> Int
calc punten = 
    let hoeken = [ (a,b) | a <- punten, b <- punten, a < b]
        leftmostpunten = take 2 $ sort punten
        
        puntenvolg = 
            let puntenunten = dropWhile (`notElem` leftmostpunten) punten
                puntens
                    | head puntenunten == head leftmostpunten = punten
                    | otherwise = reverse punten
            in take (length punten + 2) $ dropWhile (/= head leftmostpunten) $ cycle puntens

        puntendir = zipWith3 (\a b c -> (b,getpuntdir a b c)) 
            puntenvolg (drop 1 puntenvolg) (drop 2 puntenvolg)

        puntendirbb = 
            let (p1, dir1) = head puntendir
                start = (p1, dir1, True)
                f (_, dirp, binnen) (punt, dir)
                    | isopposite dirp dir = (punt, dir, not binnen)
                    | otherwise = (punt, dir, binnen)
                alless = scanl' f start puntendir
            in tail alless
        
        beste = maximumBy (comparing area) $ process hoeken puntendirbb

    in area beste

area ((x1,y1),(x2,y2)) = (1 + abs (x1-x2)) * (1 + abs (y1 - y2))


getpuntdir (xp,yp) (xc,yc) (xn,yn) =
  let (v1x,v1y) = (xc - xp, yc - yp)
      (v2x,v2y) = (xc - xn, yc - yn)
      (vx,vy) = (v1x+v2x, v1y+v2y)
  in case (vx > 0, vy > 0) of
        (True, True) -> BR
        (True, False) -> TR
        (False, True) -> BL
        (False, False) -> TL
        
inquadrant d (xb,yb) (xp,yp) = inquadrant' d (xp-xb, yp-yb)
inquadrant' TL (x,y) = x < 0 && y < 0
inquadrant' TR (x,y) = x > 0 && y < 0
inquadrant' BL (x,y) = x < 0 && y > 0
inquadrant' BR (x,y) = x > 0 && y > 0
  
opposite TL = BR
opposite TR = BL
opposite BR = TL
opposite BL = TR
isopposite a b = a == opposite b

data Dir = TL | TR | BL | BR deriving (Show, Eq)

gethoek TL ((x1,y1),(x2,y2)) = (min x1 x2, min y1 y2)
gethoek TR ((x1,y1),(x2,y2)) = (max x1 x2, min y1 y2)
gethoek BL ((x1,y1),(x2,y2)) = (min x1 x2, max y1 y2)
gethoek BR ((x1,y1),(x2,y2)) = (max x1 x2, max y1 y2)

process :: [((Int, Int), (Int, Int))] -> [((Int, Int), Dir, Bool)] -> [((Int, Int), (Int, Int))]
process hoeken [] = hoeken
process hoeken ( (_, _, True):rest ) = process hoeken rest
process hoeken ( (_, _, False):(_,_, False):(_, _, False):rest )
  = error "aaah drie op rij"
process hoeken ( (b1, dir1, False):(b2,dir2, False):rest )
  = process hoeken' rest
  where 
    inq1 p = inquadrant (opposite dir1) b1 (gethoek (opposite dir1) p)
    inq2 p = inquadrant (opposite dir2) b2 (gethoek (opposite dir2) p)
    hoeken' :: [((Int, Int), (Int, Int))]
    hoeken' = filter (\p -> not (inq1 p) || not (inq2 p)) hoeken
process hoeken ( (b, dir, False):rest )
  = process hoeken' rest
  where 
    hoeken' :: [((Int, Int), (Int, Int))]
    hoeken' = filter (\p -> not $ inquadrant (opposite dir) b (gethoek (opposite dir) p)) hoeken
--  = filter (\p -> getpuntdir' b (gethoek (opposite dir) p) == dir) hoeken
