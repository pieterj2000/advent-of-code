
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


order :: [(Int, Int)] -> [Int] -> [Int]
order rules [x] = [x]
order rules ls =
    let relevantrules = filter (\(a,b) -> a `elem` ls && b `elem` ls) rules
        notinrhs x = all ((/=x) . snd) relevantrules
        goeie = head $ filter notinrhs ls
    in goeie : order relevantrules (filter (/=goeie) ls)

calc :: ([(Int, Int)], [[Int]]) -> Int
calc (rules, updates) = sum $ map (getMiddle . order rules) $ filter (not . isGoed rules) updates