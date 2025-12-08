import Data.List (sortOn, union, foldl')
import Data.Ord (Down (..) )
import qualified Data.Set as S

inputFile = "8a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [(Int, Int, Int)]
parse = map (read . (\x -> '(' : x ++ ")") ) . lines

--calc :: [(Int, Int, Int)] -> Int
calc punten =
    let edges = [ (a,b) | a<-punten, b<-punten, a<b]
        sedges = sortOn dist edges
        
        gedaan = scanl (\(spul,_) edge -> (addinto edge spul, edge)) ([],((0,0,0),(0,0,0))) sedges
        klaar (groepen,_) = length groepen == 1 && length (head groepen) == length punten
        gedaan' = dropWhile (not . klaar) gedaan
        (_,((x1,_,_),(x2,_,_))) = head gedaan'
    in x1*x2

dist ((x1,y1,z1),(x2,y2,z2)) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

get :: Ord a => a -> [S.Set a] -> S.Set a
get a [] = S.singleton a
get a (s:ss)
  | a `S.member` s = s
  | otherwise = get a ss
  
getnot :: Ord a => (a,a) -> [S.Set a] -> [S.Set a]
getnot (a,b) [] = []
getnot (a,b) (s:ss)
  | a `S.member` s || b `S.member` s = getnot (a,b) ss
  | otherwise = s : getnot (a,b) ss

addinto :: Ord a => (a,a) -> [S.Set a] -> [S.Set a]  
addinto (a,b) spul =
  let aspul = get a spul
      bspul = get b spul
      rest = getnot (a,b) spul
  in (S.union aspul bspul) : rest
