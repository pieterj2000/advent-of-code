
-- stack script --resolver lts-21.22
import qualified Parser as P
import Control.Applicative (Alternative(..), asum)
import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Control.Monad ((>=>))

inputFile = "5a.input"

main = do
    input <- readFile inputFile
    print $ calc input


calc :: String -> Int
calc input = let (seedCriticalPoints, maps) = parseInput input
                 f = foldl1 (>=>) maps
                 g (a,b) = [a,b]
             in minimum $ seedCriticalPoints >>= f >>= g

numP :: P.Parser Int
numP = many P.space *> P.int

groupPairs :: [Int] -> [(Int, Int)]
groupPairs [] = []
groupPairs (a:b:xs) = (a,b) : groupPairs xs
groupPairs (a:xs) = error "geen even aantal seedsrange grenzen"

toBounds :: (Int, Int) -> (Int, Int)
toBounds (left, len) = (left, left + len - 1)

seedP :: P.Parser [(Int, Int)]
seedP = map toBounds . groupPairs <$> (P.string "seeds:" *> some numP)

mapLineP :: P.Parser (Int, Int, Int)
mapLineP = P.newline *> ( (\a b c -> (a,b,c)) <$> numP <*> numP <*> numP )

names = ["seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location"]

mapP :: P.Parser [(Int, Int, Int)]
mapP = let headerP = P.oneOf names *> P.string "-to-" *> P.oneOf names *> P.string " map:"
        in headerP *> some mapLineP

updateBoundsLine :: (Int, Int, Int) -> (Int, Int) ->  [(Int, Int)]
updateBoundsLine (dest, src, len) (l,r) =   let sbetween x (a,b) = (x > a) && (x < b)
                                                (bl, br) = (src, src + len)

                                                split :: (Int, Int) -> [Int] -> [(Int, Int)]
                                                split (a,b) [] = [(a,b)]
                                                split (a,b) (x:xs) = if sbetween x (a,b)
                                                                        then (a,x) : split (x,b) xs
                                                                        else split (a,b) xs
                                            in split (l,r) [bl,br]




applyMapsToBound :: [(Int, Int, Int)] -> (Int, Int) -> (Int, Int)
applyMapsToBound [] (l,r) = (l,r)
applyMapsToBound ((dest, src, len):bds) (l,r) = let between x (a,b) = (x >= a) && (x < b)
                                                    (bl, br) = (src, src + len)
                                                    applyFun x = x - src + dest
                                                in if between l (bl,br)
                                                    then (applyFun l, applyFun r)
                                                    else applyMapsToBound bds (l,r)

updateBoundsMap :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
updateBoundsMap fs = map (applyMapsToBound fs) . foldr1 (>=>) (map updateBoundsLine fs)

mapsP :: P.Parser [(Int, Int) -> [(Int, Int)]]
mapsP = some (updateBoundsMap <$> (many P.newline *> mapP))

parseInput :: String -> ([(Int,Int)], [(Int, Int) -> [(Int, Int)]])
parseInput =  P.parseResult $ (\a c b -> (a,b)) <$> seedP <*> many P.newline <*> mapsP
