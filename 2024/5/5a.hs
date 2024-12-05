
inputFile = "5a.input"

main = do
    input <- readFile inputFile
    print $ calc . parse $ input


parse :: String -> ([(Int, Int)], [[Int]])
parse input =
    let ls = lines input
        (rulels, _:updatels) = break null ls
        readrule :: String -> (Int, Int)
        readrule l = let (a,_:b) = span (/='|') l in (read a, read b)
        readupdate :: String -> [Int]
        readupdate l = read $ "[" ++ l ++ "]"
    in (map readrule rulels, map readupdate updatels)

getMiddle :: [a] -> a
getMiddle x = let l = length x; c = (l-1) `div` 2 in x !! c

isGoed :: [(Int, Int)] -> [Int] -> Bool
isGoed rules [x] = True
isGoed rules (x:xs)
    | all (`notElem` xs) rsx    = isGoed rs'' xs
    | otherwise                 = False
    where
        rs' = filter ((/=x) . fst) rules
        rsx = map fst $ filter ((==x) . snd) rs'
        rs'' = filter ((/=x) . snd) rs'

calc :: ([(Int, Int)], [[Int]]) -> Int
calc (rules, updates) = sum $ map getMiddle $ filter (isGoed rules) updates