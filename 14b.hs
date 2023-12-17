import Data.List (transpose, intercalate)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Maybe (fromJust)


inputFile = "14a.input"

main = do
    input <- readFile inputFile
    print $ calc input

-- [[Char]] is echt heel inefficient, maar boeiend
type Platform = [Row]
type Row = String
type SubRow = (String, Int)

--calc :: String -> Int
calc = calcLoadCurrentPlatform . getAtCycles 1000000000 . parseInput

calcLoadCurrentPlatform :: Platform -> Int
calcLoadCurrentPlatform = sum . map calcLoadRow
calcLoadRow :: Row -> Int
calcLoadRow row =   let l = length row
                        list = [l,(l-1)..]
                    in sum $ zipWith (\i x -> if x == 'O' then i else 0) list row


getAtCycles :: Int -> Platform -> Platform
getAtCycles i p =   let vec = iterated p
                        l = V.length vec
                        last = vec V.! (l-1)
                        f = fromJust $ V.elemIndex (doCycle last) vec
                        n = i - 1
                    in if n < f then vec V.! n 
                        else vec V.! (((n-f) `mod` (l-f)) + f)


iterated :: Platform -> V.Vector Platform
iterated start = V.unfoldrN 1000000000 f (S.empty, start) 
    where   f :: (S.Set Platform, Platform) -> Maybe (Platform, (S.Set Platform, Platform))
            f (set, prev) = let next = doCycle prev
                                nextSet = S.insert next set
                            in if next `S.member` set then Nothing
                                else Just (next, (nextSet, next))


parseInput :: String -> Platform
parseInput = transpose . lines

doCycle :: Platform -> Platform
doCycle = doEast . doSouth . doWest . doNorth

doNorth :: Platform -> Platform
doNorth = moveField
doWest :: Platform -> Platform
doWest = transpose . moveField . transpose
doSouth :: Platform -> Platform
doSouth = map reverse . moveField . map reverse
doEast :: Platform -> Platform
doEast = transpose . map reverse . moveField . map reverse . transpose




moveField :: Platform -> Platform
moveField = map moveRow

moveRow :: Row -> Row
moveRow row =   let subrows = splitBy (=='#') row
                    out = intercalate "#" $ map moveSubRow subrows
                in take (length row) $ out ++ repeat '#'

moveSubRow :: Row -> Row
moveSubRow subrow = let l = length subrow
                        r = numberRocks subrow
                        d = l-r
                    in replicate r 'O' ++ replicate d '.'

------------

calcLoad :: Platform -> Int
calcLoad = sum . map doRow

doRow :: Row -> Int
doRow = sum . map doSubRow . splitRow

doSubRow  :: SubRow -> Int
doSubRow (xs, i) = sum $ take (numberRocks xs) [i,(i-1)..]

numberRocks :: Row -> Int
numberRocks = length . filter (=='O')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = let (sub, rest) = break p xs
                in sub : splitBy p (drop 1 rest)

hasRocks :: Row -> Bool
hasRocks = elem 'O'

splitRow :: Row -> [SubRow]
splitRow r =    let len = length r
                    subs = splitBy (\(i, el) -> el == '#') $ zip [len, (len-1)..] r
                    subs' = filter (hasRocks . map snd) subs
                in map (\xs -> (map snd xs, fst $ head xs)) subs'