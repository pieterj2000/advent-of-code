import Linear.V2
import Data.List
import Data.Containers.ListUtils (nubOrd)

inputFile = "8a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> ([[V2 Int]], V2 Int -> Bool)
parse input = 
    let ls = lines input
        w = length $ head ls
        h = length ls
        addcoordsl l y = zipWith (\c x -> (c, V2 x y)) l [1..]
        withcoords = zipWith addcoordsl ls [1..]
        groups = groupBy (\x y -> fst x == fst y) $ sortOn fst $ concat withcoords
        groupsnotpoint = filter (\l -> fst (head l) /= '.') groups

        inbound (V2 x y) = x >= 1 && x <= w && y >= 1 && y <= h
    in (map (map snd) groupsnotpoint, inbound)

--calc :: [[Int]] -> Int
calc = length . antinodes 

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

antinode :: (V2 Int -> Bool) -> V2 Int -> V2 Int -> [V2 Int]
antinode inbound a b = 
    let diff = a - b 
        omhoogs = takeWhile inbound $ iterate (+diff) a
        omlaags = takeWhile inbound $ iterate (subtract diff) b
    in omhoogs ++ omlaags

antinodes :: ([[V2 Int]], V2 Int -> Bool) -> [V2 Int]
antinodes (groups, inbound) = nubOrd $ concatMap antinodesgroup groups
    where
        antinodesgroup g = concatMap (\[a,b] -> antinode inbound a b) $ choose 2 g
