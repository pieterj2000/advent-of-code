import qualified Data.Map as M
import Data.List (foldl')

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
  let dacnaarfft = looptel g "fft" "dac"
      fftnaardac = looptel g "dac" "fft"
      svrnaardac = looptel g "dac" "svr"
      svrnaarfft = looptel g "fft" "svr"
      dacnaarout = looptel g "out" "dac"
      fftnaarout = looptel g "out" "fft"
  in if dacnaarfft == 0
    then svrnaarfft * fftnaardac * dacnaarout
    else svrnaardac * dacnaarfft * fftnaarout
--  in [dacnaarfft,fftnaardac,svrnaardac,svrnaarfft,dacnaarout,fftnaarout]

looptel g naar van = 
  let neighs = g M.!? van
  in if van == naar 
    then 1 
    else case neighs of 
      Nothing -> 0
      Just ns -> sum $ map (looptel g naar) ns
  
  
  
reversegraph g =
  let insertback van naars g = foldl' (\accg naar -> M.insertWith (++) naar [van] accg) g naars
  in M.foldrWithKey insertback M.empty g