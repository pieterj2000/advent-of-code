import Data.Char (isDigit)
import Data.List (sort)

inputFile = "5a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> ([(Int, Int)], [Int])
parse spul = 
    let ls = lines spul
        (deel1, (_:deel2)) = span (not . null) ls
        pint x = let (l, ('-':r)) = span isDigit x in (read l, read r)
        intervals = map pint deel1
        getallen = map read deel2
    in (sort intervals, sort getallen)

calc (ints, getallen) = length $ loop ints getallen

loop :: [(Int, Int)] -> [Int] -> [Int]
loop _ [] = []
loop [] _ = []
loop ((l,u):restgoed) (n:ns)
  | n < l = loop ((l,u):restgoed) ns
  | n > u = loop restgoed (n:ns)
  | otherwise = n : loop ((l,u):restgoed) ns
