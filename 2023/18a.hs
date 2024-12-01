import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List (groupBy, sortOn)
import Data.Ord (comparing)
import Debug.Trace (traceShow, trace)

inputFile = "18a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
--calc = sum . map calcVol . makeRows . makeTrench . parseInput
calc = calcVol . parseInput

type Point = (Int, Int)
data Dir = U | R | D | L deriving (Show, Read, Eq, Enum, Ord)

move :: Point -> Dir -> Point
move (x,y) U = (x,y-1)
move (x,y) D = (x,y+1)
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)

parseInput :: String -> [(Dir, Int)]
parseInput input =  let ls = lines input
                        dir = read . take 1
                        dist = read . takeWhile isDigit . drop 2
                    in map (\l -> (dir l, dist l)) ls

opp :: Dir -> Dir
opp U = D
opp D = U
opp L = R
opp R = L

rightDir :: Dir -> Dir
rightDir R = D
rightDir L = U

-- R is right-oriented
-- L is left-oriented
-- werkte met oude systeem, we weten (door naar de input te kijken) dat orientatie
-- positief is (rechtsom)
getOrientation :: [[(Point, Dir)]] -> Dir
--getOrientation (r:_) = head $ filter (\d -> d == L || d == R) $ map snd r
getOrientation _ = R

calcVol :: [(Dir, Int)] -> Int
calcVol ls@((d,_):_) = calcVol' 0 d (ls ++ [(d,0)])

calcVol' :: Int -> Dir -> [(Dir, Int)] -> Int
calcVol' x prevD [a] = 0
calcVol' x prevD ds@((d,dist):(nextD,_):_) = traceShow (x, d, dist) $ case d of
        L -> calcVol' (x - dist) d (tail ds)
        R -> calcVol' (x + dist) d (tail ds)
        _ ->
            let orientation = getOrientation []
                (x', sign) = if rightDir orientation == d then (x + 1, 1) else (x,-1)
                dist'
                  | prevD == nextD = dist
                  | prevD == orientation = dist + sign
                  | otherwise = dist - sign
            in traceShow (x', dist'*x'*sign) $ dist'*x'*sign + calcVol' x d (tail ds)



-- WERKT HELAAS NIET, GEEFT TE HOOG
---     
---     parseInput :: String -> [(Dir, Int)]
---     parseInput input =  let ls = lines input
---                             dir = read . take 1
---                             dist = read . takeWhile isDigit . drop 2
---                         in map (\l -> (dir l, dist l)) ls
---     
---     
---     makeTrench :: [(Dir, Int)] -> [(Point, Dir)]
---     makeTrench = doe (0,0)
---         where   doe :: Point -> [(Dir, Int)] -> [(Point, Dir)]
---                 doe _ [] = []
---                 doe p ((dir', dist):xs)
---                     | dist == 1 = (p', dir') : doe p' xs
---                     | otherwise = (p', dir') : doe p' ((dir', dist - 1):xs)
---                     where p' = move p dir'
---     
---     
---     makeRows :: [(Point, Dir)] -> [[(Point, Dir)]]
---     makeRows = map (sortOn (fst . fst)) . groupBy (\((x,y),_) ((x2,y2),_) -> y == y2) . sortOn (snd . fst)
---     
---     
---     
---     -- R is right-oriented
---     -- L is left-oriented
---     getOrientation :: [[(Point, Dir)]] -> Dir
---     getOrientation (r:_) = head $ filter (\d -> d == L || d == R) $ map snd r
---     
---     calcVol :: Dir -> [(Point, Dir)] -> Int
---     calcVol orientation ls =    let dirL = leftBnd orientation
---                                     dirR = opp dirL
---     
---                                     doe :: [(Int, Dir)] -> Int
---                                     doe [] = 0
---                                     doe ((_,L):ds) = 1 + doe ds
---                                     doe ((_,R):ds) = 1 + doe ds
---                                     doe alles@((iL,l):ds)
---                                         | l == dirR = 1 + doe ds
---                                         | l == dirL = if geenR
---                                                             then if metOrientatie
---                                                                 then aantalTrenchTotVolgendeL + doe vanafVolgendeL
---                                                                 else afstandTotVolgendeL + doe vanafVolgendeL
---     
---                                                             else afstandTotLaatsteR + doe vanafLaatsteR
---                                         where
---                                             (totVolgdendeL, vanafVolgendeL) = span ((/=dirL) . snd) ds
---                                             indexVolgendeL = fst $ head vanafVolgendeL
---                                             geenR = not (any ((==dirR) . snd) totVolgdendeL)
---                                             metOrientatie = orientation == head (map snd ds)
---                                             aantalTrenchTotVolgendeL = 1 + length totVolgdendeL
---                                             afstandTotVolgendeL = indexVolgendeL - iL
---                                             indexLaatsteR = fst . last $ filter ((==dirR) . snd) totVolgdendeL
---                                             (totLaatsteR, vanafLaatsteR) = span (\(i,d) -> i < indexLaatsteR) alles
---                                             afstandTotLaatsteR = indexLaatsteR - iL
---     
---                                 in doe $ map (\((x,y),d) -> (x,d)) ls

printTrench :: [(Point, Dir)] -> String
printTrench ps =    let minW = minimum $ map (fst . fst) ps
                        maxW = maximum $ map (fst . fst) ps
                        minH = minimum $ map (snd . fst) ps
                        maxH = maximum $ map (snd . fst) ps
                    in unlines [ [ maybe '.' (head . show) (lookup (x,y) ps) | x<-[minW..maxW] ] | y<-[minH..maxH]]