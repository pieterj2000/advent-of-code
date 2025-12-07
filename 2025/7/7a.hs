import Data.List (unfoldr, zipWith6)
import Control.Arrow (second)

inputFile = "7a.input"
main = do
    input <- readFile inputFile
    mapM_ (print . (map $ (\x -> if x then 1 else 0))) $ parse $ input
    mapM_ (print . (second $ map $ (\x -> if x then 1 else 0))) $ calc . parse $ input
    print $ calc . parse $ input
    

parse :: String -> [[Bool]]
parse = map (map (=='^')) . lines

--calc :: [[Bool]] -> [Int]
calc spul = 
    let len = length $ head spul
        len2 = len `div` 2
        startvec = replicate len2 False ++ [True] ++ replicate len2 False
    in scanl fun (0, startvec) spul


fun :: (Int, [Bool]) -> [Bool] -> (Int, ([Bool]))
fun (count, huidig) splitters =
    let splitters' = False : splitters ++ [False]
        huidig' = False : huidig ++ [False]
        nieuw = zipWith6 f huidig' (drop 1 huidig') (drop 2 huidig') splitters' (drop 1 splitters') (drop 2 splitters')

        f _ _ _ _ True _ = False -- er zit een splitter op de plek, kan nooit signaal zijn
        f _ True _ _ False _ = True -- wel signaal direct boven, geen splitter op de plak => signaal gaat door
        f True _ _ True _ _ = True -- splitter links én signaal erin => signaal op deze plek
        f _ _ True _ _ True = True -- zelfde maar rechts
        f _ _ _ _ _ _ = False

        veranderd = sum $ zipWith (\a b -> if a && not b then 1 else 0) huidig nieuw -- er was een signaal, nu niet meer => splitter gebruikt

    in (count + veranderd, nieuw)