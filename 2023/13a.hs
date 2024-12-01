import Data.List (transpose)

inputFile = "13.input"

main = do
    input <- readFile inputFile
    print $ calc input

type Field = [[Char]]

--calc :: String -> Int
calc = sum . map scoreField . parseInput

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
horizontalReflectionOptions f = map fst $ filter snd $ zip [1..] $ zipWith (==) f (tail f)

isHorizontalReflection :: Field -> Int -> Bool
isHorizontalReflection field r = and $ zipWith (==) (reverse $ take r field) (drop r field)