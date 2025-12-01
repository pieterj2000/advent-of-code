
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
calc = length . filter (==0) . scanl ( (`mod` 100) .: (+)) 50