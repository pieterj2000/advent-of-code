import qualified Data.Map as M
import Data.List (foldl', union)

inputFile = "11a.input"
main = do
    input <- readFile inputFile
    print $ calc . parse $ input

parse :: String -> M.Map [Char] [String]
parse = M.fromList . map pding . lines

pding :: [Char] -> ([Char], [String])
pding spul = 
    let naam = take 3 spul
        rest = drop 5 spul
    in (naam, words rest)
   
calc :: M.Map String [String] -> Int 
calc g = 
    let dacnaarfft = distvannaar g "dac" "fft"
        fftnaardac = distvannaar g "fft" "dac"
        svrnaardac = distvannaar g "svr" "dac"
        svrnaarfft = distvannaar g "svr" "fft"
        dacnaarout = distvannaar g "dac" "out"
        fftnaarout = distvannaar g "fft" "out"
    in if dacnaarfft == 0
        then svrnaarfft * fftnaardac * dacnaarout
        else svrnaardac * dacnaarfft * fftnaarout
    --  in [dacnaarfft,fftnaardac,svrnaardac,svrnaarfft,dacnaarout,fftnaarout]
    --    in distvannaar g "svr" "ccc"

looptel g naar van = 
    let neighs = g M.!? van
    in if van == naar 
        then 1 
        else case neighs of 
            Nothing -> 0
            Just ns -> sum $ map (looptel g naar) ns
    
distvannaar g van naar =
    let order = toporder g "svr"
        ivan = length $ takeWhile (/=van) order
        inaar = length $ takeWhile (/=naar) order
        order' = takeWhile (/= naar) $ dropWhile (/= van) order
        order'' = reverse order'
    in if van == naar
        then 1
        else if ivan > inaar 
            then 0
            else (distvannaar' g (M.singleton naar 1) order'') M.! van
  
distvannaar' g m [] = m
distvannaar' g m (n:ns) = 
    let volgenden = g M.! n
        val = sum $ map (\n -> M.findWithDefault 0 n m) volgenden
        m' = M.insert n val m
    in distvannaar' g m' ns

reversegraph g =
    let insertback van naars g = foldl' (\accg naar -> M.insertWith (++) naar [van] accg) g naars
    in M.foldrWithKey insertback M.empty g


toporder g start = 
    let reverseg = reversegraph g
        doe :: M.Map String [String] -> [String] -> [String]
        doe _ [] = []
        doe rg (s:ss) = s : doe rg'' ss'
            where
                volgenden :: [String]
                volgenden = M.findWithDefault [] s g
                rg' :: M.Map String [String]
                rg' = M.delete s rg
                rg'' = foldr (M.adjust (filter (/=s))) rg' volgenden
                ss' :: [String]
                ss' = union (map fst . M.toList $ M.filter null rg'') ss
    in doe reverseg [start]