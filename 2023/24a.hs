import qualified Parser as P
import Control.Applicative (Alternative(..))

inputFile = "24a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = length . filter id . map (uncurry intersectsInZone) . pairs . parseInput

type V3 a = (a, a, a)
type V2 a = (a, a)

parseInput :: String -> [(V3 Integer, V3 Integer)]
parseInput input =
    let sepP = (,) <$> P.char ',' <*> many P.space
        pointP = (\a _ b _ c -> (a,b,c)) <$> P.signedInt <*> sepP <*> P.signedInt <*> sepP <*> P.signedInt
        dingP = (,) <$> pointP <*> (P.string " @" *> many P.space *> pointP)
    in map (P.parseResult dingP) $ lines input

getIntersection :: (V3 Integer, V3 Integer) -> (V3 Integer, V3 Integer) -> Maybe (Rational, Rational, Rational, Rational)
getIntersection ((x1, y1, _), (dx1, dy1, _)) ((x2, y2, _), (dx2, dy2, _))
    | denom == 0 = Nothing
    | otherwise = Just (x,y,t1,t2)
        where
            [x1',y1',dx1',dy1',x2',y2',dx2',dy2'] = map fromIntegral [x1,y1,dx1,dy1,x2,y2,dx2,dy2] :: [Rational]
            denom = dy1'*dx2' - dy2'*dx1'
            t1 = ((x1'-x2')*dy2' - (y1'-y2')*dx2') / denom
            t2 = ((x1'-x2')*dy1' - (y1'-y2')*dx1') / denom
            x = x1'+dx1'*t1
            y = y1'+dy1'*t1

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

intersectsInZone :: (V3 Integer, V3 Integer) -> (V3 Integer, V3 Integer) -> Bool
intersectsInZone a b = case getIntersection a b of
    Nothing -> False
    Just (x,y,t1,t2) -> between x && between y && t1 >= 0 && t2 >= 0
        where
            l = 200000000000000
            u = 400000000000000
            between n = n >= l && n <= u

