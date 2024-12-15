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

maakDisk :: [Int] -> [Maybe Int]
maakDisk = doe [0..] True
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

defragment :: [Maybe Int] -> [Int]
defragment xs = doe 0 (length xs) xs (reverse xs)
    where
        doe :: Int -> Int -> [Maybe Int] -> [Maybe Int] -> [Int] -- vanaf links -> vanaf rechts -> output
        doe _ _ [] _ = []
        doe _ _ _ [] = []
        doe il ir (Just l:ls) rs = if il >= ir then [] else l : doe (il+1) ir ls rs
        doe il ir ls (Nothing:rs) = if il >= ir then [] else doe il (ir-1) ls rs
        doe il ir (Nothing:ls) (Just r:rs)
            | il >= ir = []
            | otherwise = r : doe (il+1) (ir-1) ls rs

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]