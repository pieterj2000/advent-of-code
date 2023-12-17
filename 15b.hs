import Data.Char (ord, isLetter)
import Data.List (foldl', sortOn, groupBy)
import Data.Ord (comparing)

inputFile = "15a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = sum . map (scoreBox . processBox) . groupEvents . parseInput

data Lens = Lens { label :: String, strength :: Int }  deriving (Show, Eq, Read)
data Ding = Add { lens :: Lens } | Rem { lens :: Lens } deriving (Show, Eq, Read)

readDing :: String -> Ding
readDing xs =   let (label,rest) = span isLetter xs
                in if head rest == '='
                    then Add $ Lens label (read $ tail rest)
                    else Rem $ Lens label 0

groupEvents :: [Ding] -> [[Ding]]
groupEvents = groupBy (\a b -> hash (label $ lens a) == hash (label $ lens b)) . sortOn (hash . label . lens)


doDing :: [Lens] -> Ding -> [Lens]
doDing []       (Add l) = [l]
doDing (x:xs)   (Add l) = if label x == label l then l:xs else x : doDing xs (Add l)
doDing []       (Rem l) = []
doDing (x:xs)   (Rem l) = if label x == label l then xs else x : doDing xs (Rem l)

processBox :: [Ding] -> [Lens]
processBox = foldl' doDing []

scoreLens :: Int -> Lens -> Int
scoreLens slot lens = (1 + hash (label lens)) * slot * (strength lens)

scoreBox :: [Lens] -> Int
scoreBox = sum . zipWith scoreLens [1..]

hash :: String -> Int
hash = foldl' (\acc el -> (17*(acc + ord el)) `mod` 256) 0

parseInput :: String -> [Ding]
parseInput = map readDing . splitBy (==',')

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs = let (sub, rest) = break p xs
                in sub : splitBy p (drop 1 rest)