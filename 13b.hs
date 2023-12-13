import Data.List (transpose)

inputFile = "13.input"

main = do
    input <- readFile inputFile
    print $ calc input

type Field = [[Char]]

--calc :: String -> Int
calc = sum . map scoreField . parseInput
--calc = map getVerticalReflections . parseInput

scoreField :: Field -> Int
scoreField f =  let verts = getVerticalReflections f
                    horts = getHorizontalReflections f
                    vertsP = verts
                    hortsP = map (*100) horts
                in sum hortsP + sum vertsP

parseInput :: String -> [Field]
parseInput input = doe $ lines input
    where   doe :: [String] -> [Field]
            doe [] = []
            doe ls = takeWhile (/="") ls : doe (drop 1 $ dropWhile (/="") ls)

getVerticalReflections :: Field -> [Int]
getVerticalReflections = getHorizontalReflections . transpose

getHorizontalReflections :: Field -> [Int]
getHorizontalReflections f = filter (isHorizontalReflection f) $ horizontalReflectionOptions f

horizontalReflectionOptions :: Field -> [Int]
horizontalReflectionOptions f = map fst $ filter snd $ zip [1..] $ zipWith atMost1DiffRow f (tail f)

isHorizontalReflection :: Field -> Int -> Bool
isHorizontalReflection field r = sum (zipWith countDiffRow (reverse $ take r field) (drop r field)) == 1

countDiffRow :: String -> String -> Int
countDiffRow row1 row2 = length $ filter id $ zipWith (/=) row1 row2

atMost1DiffRow :: String -> String -> Bool
atMost1DiffRow a b =  countDiffRow a b <= 1



