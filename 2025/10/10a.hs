import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.Bits (shift, xor, (.|.))


inputFile = "10a.input"
main = do
    input <- readFile inputFile
    --print $ parse input
    print $ calc . parse $ input


parse :: String -> [([Bool], [[Int]])]
parse = map pline . lines

pline :: String -> ([Bool], [[Int]])
pline spul =
    let blinks = map (=='#') $ tail $ takeWhile (/=']') spul
        spul' = map (\c -> if c == '(' then '[' else if c == ')' then ']' else c) spul
        dinges = init $ tail $ words spul'
    in (blinks, map read dinges)

--calc :: [[Int]] -> Int
calc = sum . map calci

calci (doel, combs) = 
    let doelnum = foldl' (\acc b -> (shift acc 1) .|. (if b then 1 else 0)) 0 $ reverse doel
        buttons = map combtonums combs
        pressvecs = vecs $ length combs
        pressvecs' = sortBy (comparing sum) pressvecs
        resultaten = map (\press -> (press, applyButtons buttons press)) pressvecs'
    in sum . fst . head . filter ((==doelnum) . snd) $ resultaten


combtonums :: [Int] -> Int
combtonums = foldl' (\acc n -> acc `xor` (shift 1 n)) 0

applyButtons :: [Int] -> [Int] -> Int
applyButtons buttons presses = foldl' xor 0 . map fst $ filter ((==1) . snd) $ zip buttons presses


vecs :: Int -> [[Int]]
vecs 0 = [[]]
vecs n = let rest = vecs (n-1) in map (1:) rest ++ map (0:) rest



-- misschien geen slechte, want deze genereerd alleen die met 0, maar goed
-- vecs :: Int -> Int -> [[Int]]
-- vecs len 0 = [replicate len 0]
-- vecs len n
--     | n > len = []
--     | otherwise = 
--         let met1 = map (1:) $ vecs (len - 1) (n - 1)
--             met0 = map (0:) $ vecs (len - 1) n
--         in []