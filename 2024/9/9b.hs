import Data.Char (digitToInt, intToDigit)
import Data.Maybe (catMaybes)

inputFile = "9a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [Int]
parse = map digitToInt

--calc :: [[Int]] -> Int
calc = checksum . defragment . maakDisk


maakDisk :: [Int] -> [Entry]
maakDisk = doe 0 [0..] True
    where
        doe :: Int -> [Int] -> Bool -> [Int] -> [Entry]
        doe loc _ _ [] = []
        doe loc (i:is) True (n:ns) = Segment i n loc : doe (loc+n) is False ns
        doe loc is False (n:ns) = Gap n loc : doe (loc+n) is True ns

maakDisk' :: [Int] -> [Maybe Int]
maakDisk' = doe [0..] True
    where
        doe :: [Int] -> Bool -> [Int] -> [Maybe Int]
        doe _ _ [] = []
        doe (i:is) True (n:ns) = replicate n (Just i) ++ doe is False ns
        doe is False (n:ns) = replicate n Nothing ++ doe is True ns

printDisk :: [Maybe Int] -> String
printDisk = map doe
    where
        doe Nothing = '.'
        doe (Just i) = intToDigit i

printDisk' :: [Int] -> String
printDisk' = map intToDigit

data Entry = Segment Int Int Int | Gap Int Int deriving (Show, Read, Eq) -- Segment id length loc, Gap len loc

defragment :: [Entry] -> [Entry]
defragment xs = doe xs (reverse xs) []
    where
        doe :: [Entry] -> [Entry] -> [Entry] -> [Entry]
        doe [] _ end = [] --end
        doe _ [] end = [] --end
        doe (Segment a b c : ls) rs end = Segment a b c : doe ls rs end
        doe (Gap 0 _ : ls) rs end = doe ls rs end
        doe ls (Gap _ _ : rs) end = doe ls rs end
        doe ls (Segment id len sloc : rs) end
            | curgloc >= sloc = ls
            | otherwise = doe nieuwls rs nieuwend
                where
                    curgloc = case head ls of
                        Segment _ _ l -> l
                        Gap _ l -> l
                    nieuwls = if shift then nieuwls' else ls
                    nieuwend = if shift then end else Segment id len sloc : end
                    isgoedgap (Segment _ _ _) = False
                    isgoedgap (Gap w _) = w >= len
                    shift = any isgoedgap ls && gloc < sloc
                    prels = takeWhile (not . isgoedgap) ls
                    afters = tail $ dropWhile (not . isgoedgap) ls
                    (Gap w gloc) = head $ dropWhile (not . isgoedgap) ls
                    nieuwls' = filter (/= Segment id len sloc) $ prels ++ ((Segment id len gloc) : (Gap (w-len) (gloc+len)) : afters)



-- defragment :: [Int] -> [Int]
-- defragment xs = doe 2 (length xs) (tail xs) (reverse xs)
--     where
--         doe :: [Int] -> [Int] -> [Int]
--         doe [] _ = []
--         doe _ [] = []
--         doe il ir (l:l2:ls) (r:r2:rs)
--             | il >= ir = []
--             | otherwise = []

--         vul :: Int -> Int -> [Int] -> [Int] -> ([Int],[Int])
--         vul maxindex len [] _ = ([],[])
--         vul maxindex len (g:_:gs) (r:rs)
--             | g < len   = g : vul maxindex len gs
--             | otherwise = 

defragment' :: [Maybe Int] -> [Int]
defragment' xs = doe 0 (length xs) xs (reverse xs)
    where
        doe :: Int -> Int -> [Maybe Int] -> [Maybe Int] -> [Int] -- vanaf links -> vanaf rechts -> output
        doe _ _ [] _ = []
        doe _ _ _ [] = []
        doe il ir (Just l:ls) rs = if il >= ir then [] else l : doe (il+1) ir ls rs
        doe il ir ls (Nothing:rs) = if il >= ir then [] else doe il (ir-1) ls rs
        doe il ir (Nothing:ls) (Just r:rs)
            | il >= ir = []
            | otherwise = r : doe (il+1) (ir-1) ls rs


checksum :: [Entry] -> Int
checksum = sum . map checksumseg
    where
        checksumseg (Segment id len loc) = sum $ take len $ map (*id) [loc..]
        checksumseg (Gap _ _) = 0

checksum' :: [Int] -> Int
checksum' = sum . zipWith (*) [0..]