
-- stack script --resolver lts-21.22
import qualified Parser as P
import Data.Char (isDigit)
import Control.Applicative (Alternative(..))


inputFile = "6a.input"

main = do
    input <- readFile inputFile
    print $ calc input


calc :: String -> Int
calc = possibilities . parseInput

parseInput :: String -> (Int, Int)
parseInput =    let num = many P.space *> P.int
                    parse = P.parseResult $ some num
                in (\(x:y:xs) -> (x,y)) . map (read . filter isDigit) . lines

possibilities :: (Int, Int) -> Int
possibilities (t, c) = let  tF = fromIntegral t :: Double
                            cF = fromIntegral c :: Double
                            d = sqrt (tF*tF-4*cF)
                            lF = (tF - d) / 2 + 1
                            rF = (tF + d) / 2 - 1
                            l = floor lF
                            r = ceiling rF
                        in r - l + 1