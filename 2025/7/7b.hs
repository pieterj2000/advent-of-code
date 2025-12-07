import Data.List (unfoldr, zipWith6)
import Control.Arrow (second)

inputFile = "7a.input"
main = do
    input <- readFile inputFile
    --mapM_ (print . (map $ (\x -> if x then 1 else 0))) $ parse $ input
    --mapM_ print $ calc . parse $ input
    print $ calc . parse $ input
    

parse :: String -> [[Bool]]
parse = map (map (=='^')) . lines

--calc :: [[Bool]] -> [Int]
calc spul = 
    let len = length $ head spul
        len2 = len `div` 2
        startvec = replicate len2 0 ++ [1] ++ replicate len2 0
    in sum $ foldl' fun startvec spul


fun :: [Int] -> [Bool] -> [Int]
fun huidig splitters =
    let splitters' = False : splitters ++ [False]
        huidig' = 0 : huidig ++ [0]
        nieuw = zipWith6 f huidig' (drop 1 huidig') (drop 2 huidig') splitters' (drop 1 splitters') (drop 2 splitters')

        f _ _ _ _ True _ = 0 -- er zit een splitter op de plek, kan nooit signaal zijn
        f l n r ls False rs = n + (if ls then l else 0) + (if rs then r else 0)

    in nieuw