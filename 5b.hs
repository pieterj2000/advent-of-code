
-- stack script --resolver lts-21.22
import qualified Parser as P
import Control.Applicative (Alternative(..), asum)
import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

inputFile = "5a.input"

main = do
    input <- readFile inputFile
    print $ calc input


calc :: String -> Int
calc input = let (seeds, maps) = parseInput input
                 f = foldr1 (.) . reverse $ maps
             --in minimumBy (compare `on` f) seeds
             in minimum . map f $ seeds

numP :: P.Parser Int
numP = many P.space *> P.int

groupPairs :: [Int] -> [(Int, Int)]
groupPairs [] = []
groupPairs (a:b:xs) = (a,b) : groupPairs xs
groupPairs (a:xs) = error "geen even aantal seedsrange grenzen"

toRange :: (Int, Int) -> [Int]
toRange (start, len) = [start..(start+len-1)]

seedP :: P.Parser [Int]
seedP = concat . map toRange . groupPairs <$> (P.string "seeds:" *> some numP)

mapLineP :: P.Parser (Int, Int, Int)
mapLineP = P.newline *> ( (\a b c -> (a,b,c)) <$> numP <*> numP <*> numP )

names = ["seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location"]

mapP :: P.Parser [(Int, Int, Int)]
mapP = let headerP = P.oneOf names *> P.string "-to-" *> P.oneOf names *> P.string " map:"
        in headerP *> some mapLineP

applyMap :: Int -> [(Int, Int, Int)] -> Int
applyMap x = let    doLine :: Int -> (Int, Int, Int) -> Maybe Int
                    doLine x (dest, src, len) = if (x >= src) && (x < src + len)
                        then Just (x - src + dest) else Nothing
             in  fromMaybe x . asum . map (doLine x)

mapsP :: P.Parser [Int -> Int]
mapsP = some (flip applyMap <$> (many P.newline *> mapP))

parseInput :: String -> ([Int], [Int -> Int])
parseInput =  P.parseResult $ (\a c b -> (a,b)) <$> seedP <*> many P.newline <*> mapsP
