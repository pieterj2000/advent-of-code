
inputFile = "1a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> [Int]
parse = map doe . lines
    where
        doe ('L':rest) = (-1) * (read rest)
        doe ('R':rest) = read rest

(.:) = (.) . (.)

calc :: [Int] -> Int
calc = fst . last . scanl fun (0, 50)

fun :: (Int, Int) -> Int -> (Int, Int)
fun (count, val) inc = 
    let val' = val + inc
        (k, r) = val' `divMod` 100
        k'  | val == 0 && inc < 0   = abs k - 1
            | r == 0 && inc < 0     = abs k + 1
            | otherwise             = abs k
    in (count + k', r)