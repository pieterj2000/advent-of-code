import Data.List (tails, transpose)

inputFile = "4a.input"

main = do
    input <- readFile inputFile
    print $ calc . parse $ input


parse :: String -> [[Char]]
parse = lines

horizontals :: [[a]] -> [[a]]
horizontals = concatMap doeline
    where
        doeline = filter ((==4) . length) . map (take 4) . tails

verticals :: [[a]] -> [[a]]
verticals = horizontals . transpose

diagonalsr :: [[a]] -> [[a]]
diagonalsr = concatMap vdiags . transpose . map tails
    where
        diag = take 4 . map head . filter (not . null) . zipWith drop [0..]
        vdiags = filter ((==4) . length) . map diag . tails

diagonalsl :: [[a]] -> [[a]]
diagonalsl = diagonalsr . map reverse

possibilities :: [[a]] -> [[a]]
possibilities x = concatMap ($ x) [horizontals, verticals, diagonalsl, diagonalsr]

calc :: [[Char]] -> Int
calc = length . filter (\s -> s == "XMAS" || s == "SAMX") . possibilities