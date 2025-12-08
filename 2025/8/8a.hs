import Data.List (sortOn, union, foldl')
import Data.Ord (Down (..) )

inputFile = "8a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [(Int, Int, Int)]
parse = map (read . (\x -> '(' : x ++ ")") ) . lines

--calc :: [(Int, Int, Int)] -> Int
calc punten =
    let edges = [ (a,b) | a<-punten, b<-punten, a<b]
        sedges = take (if 'x' `elem` inputFile then 10 else 1000) $ sortOn dist edges
        
        gedaan = foldl' (flip addinto) [] sedges
        driegrootste = take 3 $ sortOn Down $ map length gedaan
    in product driegrootste

dist ((x1,y1,z1),(x2,y2,z2)) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

get a [] = [a]
get a (s:ss)
  | a `elem` s = s
  | otherwise = get a ss
  
getnot (a,b) [] = []
getnot (a,b) (s:ss)
  | a `elem` s || b `elem` s = getnot (a,b) ss
  | otherwise = s : getnot (a,b) ss
  
addinto (a,b) spul =
  let aspul = get a spul
      bspul = get b spul
      rest = getnot (a,b) spul
  in (union aspul bspul) : rest
