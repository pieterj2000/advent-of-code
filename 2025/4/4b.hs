import Data.List (transpose)

inputFile = "4a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input
    --mapM_ print $ calc . parse $ input

parse :: String -> [[Int]]
parse = map (map (\c -> if c == '@' then 1 else 0)) . lines

--calc :: [[Int]] -> [[Bool]]
calc = doen

doen arr = 
    let steps = tail $ iterate (doe . map (map (\n -> if n == 2 then 0 else n))) arr
        steps' = map (map (filter (==2))) steps
        steps'' = map (sum . map sum ) steps'
    in (`div` 2) . sum $ takeWhile (/=0) steps'' -- de 'div 2'  moet omdat we alle verwijderde dingen met 2 hebben gemarkeerd
        
doe arr = 
    let arrpadupdown = repeat 0 : arr ++ [repeat 0]
        arrpad = map (\r -> 0 : r ++ [0]) arrpadupdown

        convolverows :: ([[a]] -> [b]) -> [[a]] -> [[b]]
        convolverows f spul = zipWith3 (\a b c -> f [a,b,c]) spul (drop 1 spul) (drop 2 spul)
        --convolvecols :: ([a] -> b) -> [[a]] -> [b]
        convolvecols f spul = zipWith3 (\a b c -> f $ concat [a,b,c]) (t 0) (t 1) (t 2)
            where
                spul' = takeWhile ((==3) . length) $ transpose spul
                t n = drop n spul'
        convolve f = convolverows (convolvecols f)
    
        funcy spul = convolve (\l -> let centre = l !! 4 in if centre == 1 && sum l - centre < 4 then 2 else centre) spul
    in funcy arrpad
