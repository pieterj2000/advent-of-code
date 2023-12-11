import Data.List (mapAccumL, sortOn, subsequences)

-- stack script --resolver lts-21.22



inputFile = "11a.input"

main = do
    input <- readFile inputFile
    print $ calc input

calc :: String -> Int
calc = sum . map dist . combinations . inflateUni . parseInput
--calc = length . combinations . inflateUni . parseInput

type Point = (Int, Int)

parseInput :: String -> [Point]
parseInput input =  let width = length $ takeWhile (/='\n') input
                        height = length $ lines input
                        gs = filter ((== '#') . snd) $ zip [0..] $ filter (/='\n') input
                    in map ((`quotRem` width) . fst) gs

doRows :: [Point] -> [Point]
-- accumulator is (xOld, skip), new acc is (x, skip + max (x - xOld - 1, 0) )
doRows = snd . mapAccumL (\(xOld, skip) (x,y) -> let newSkip = skip + max (x-xOld-1) 0 in ( (x, newSkip), (x + newSkip,y) ) ) (0,0)

doCols :: [Point] -> [Point]
-- accumulator is (yOld, skip), new acc is (y, skip + max (y - yOld - 1, 0) )
doCols = snd . mapAccumL (\(yOld, skip) (x,y) -> let newSkip = skip + max (y-yOld-1) 0 in ( (y, newSkip), (x,y + newSkip) ) ) (0,0)

inflateUni :: [Point] -> [Point]
inflateUni = doRows . sortOn fst . doCols . sortOn snd

combinations :: [a] -> [(a, a)]
combinations [] =  []
combinations (x:xs) = map (\a -> (x,a)) xs ++ combinations xs
                

dist :: (Point, Point) -> Int
dist ((x1,y1),(x2,y2)) = abs (x1-x2) + abs (y1-y2)

