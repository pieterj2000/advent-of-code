import Data.List (intersperse, intercalate)
import Data.Array ((!), Array, array, listArray, elems, range, bounds)

inputFile = "12.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = sum . map (uncurry count') . parseInput


parseRight :: String -> [Int]
parseRight [] = []
parseRight xs = read (takeWhile (/=',') xs) : parseRight (tailif $ dropWhile (/=',') xs)
    where tailif [] = []
          tailif (x:xs) = xs

do5times :: (String, [Int]) -> (String, [Int])
do5times (m,c) = (intercalate "?" (replicate 5 m), concat $ replicate 5 c )

parseInput :: String -> [(String, [Int])]
parseInput = take 2000 . map (do5times . (\l -> (takeWhile (/=' ') l, parseRight $ tail $ dropWhile (/=' ') l) )) . lines


count' :: String -> [Int] -> Int
count' ms cs = count 0 0 (-1)
    where   mArr = listArray (0,length ms - 1) ms
            cArr = listArray (0,length cs) ((-1):cs)

            -- string, groups, posM, posC, c
            count :: Int -> Int -> Int -> Int
            count posM posC c   | posM == length mArr                 = if all (<=0) (drop (posC+1) $ elems cArr) && c <= 0 then 1 else 0   -- einde string, zouden niet nog # nodig moeten zijn
                                | posC == length cArr - 1 && c <= 0   = if all (/='#') (drop posM $ elems mArr) then 1 else 0               -- einde groups, de rest van de string zou niet # moeten zijn
                                | mArr ! posM == '.' && c > 0        = 0                                                 -- still needs #, so error
                                | mArr ! posM == '.' && c == 0       = countMem ! (posM + 1, posC, -1)                  -- first . after #
                                | mArr ! posM == '.' && c == -1      = countMem ! (posM + 1, posC, -1)                  -- . somewhere not interesting
                                | mArr ! posM == '#' && c > 0        = countMem ! (posM + 1, posC, c-1)                 -- in # sequence
                                | mArr ! posM == '#' && c == 0       = 0                                                 -- just finished # sequence, so too many #'s
                                | mArr ! posM == '#' && c == -1      = countMem ! (posM, posC + 1, cArr ! (posC + 1))    -- start new sequence
                                | mArr ! posM == '?' && c > 0        = countMem ! (posM + 1, posC, c-1)                 -- busy with # sequence, so consider as #
                                | mArr ! posM == '?' && c == 0       = countMem ! (posM + 1, posC, -1)                  -- just finished # sequence, so must be .
                                | mArr ! posM == '?' && c == -1      = countMem ! (posM + 1, posC, -1) +                -- consider as . and continue
                                                                    countMem ! (posM, posC + 1, cArr ! (posC + 1))    -- consider as # and start new sequence

            countMem :: Array (Int, Int, Int) Int
            countMem =  listArray bnds [ count posM posC c | (posM,posC,c)<-range bnds]
                where   (mMin, mMax) = bounds mArr
                        (cMin, cMax) = bounds cArr
                        (gMin, gMax) = (-1,20)
                        bnds = ((mMin,cMin,gMin),(mMax+1,cMax,gMax))
                