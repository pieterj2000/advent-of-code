
-- stack script --resolver lts-21.22
import qualified Data.Array as A
import Data.Array ((!))
import Control.Monad.Reader
import Data.Char (isDigit)
import Data.List (sortOn)

inputFile = "3a.input"

main = do
    input <- readFile inputFile
    print $ runReader calc $ parseField input


type Point = (Int, Int)
type Field = A.Array Point Char
type R a = Reader Field a
type Number = (Int, Int, Int) -- y, xleft, xright

parseField :: String -> Field
parseField input =  let height = length $ lines input
                        width = length . head $ lines input
                    in A.array ((1,1), (width, height)) . concat $ zipWith (\y xs -> map (\(x,c) -> ((x,y),c)) xs)  [1..] (map (zip [1..]) $ lines input)

calc :: R Int
calc = getNumbers >>= filterM isPartGear >>= mapM getPartGear >>= groupGears >>= mapM gearRatio >>= return . sum

grouptwo :: [(Number, Point)] -> [((Number, Point),(Number, Point))]
grouptwo [] = []
grouptwo (x:y:xs) = if (snd x) /= (snd y) then grouptwo (y:xs) else (x,y) : grouptwo xs

groupGears :: [(Number, Point)] -> R [(Number, Number)]
groupGears xs = return $ map (\((a,_),(b,_)) -> (a,b)) $ grouptwo $ sortOn snd xs

isPartGear :: Number -> R Bool
isPartGear number = getNeighbours number >>= return . any (=='*')

getPartGear :: Number -> R (Number, Point)
getPartGear number = getNeighbourStars number >>= return . head >>= (\p -> return (number,p))

getNeighbourStars :: Number -> R [Point]
getNeighbourStars (y,xleft,xright) = do
        points <- filterM validPoint ((xleft-1,y):(xright+1,y):[(x,yn) | x<-[(xleft-1)..(xright+1)], yn<-[y-1,y+1] ])
        field <- ask
        return $ map fst $ filter ((=='*') . snd) $ map (\p -> (p, field ! p)) points


gearRatio :: (Number, Number) -> R Int
gearRatio (a,b) = do
    an <- toInt a
    bn <- toInt b
    return (an*bn)


toInt :: Number -> R Int
toInt (y, xleft, xright) = reader $ \field -> read ([field ! (x,y) | x<-[xleft..xright]])

getNumbers :: R [Number]
getNumbers = do
        field <- ask
        let ((xl,yl),(xr,yr)) = A.bounds field
            getNums :: [(Point,Char)] -> [[(Point,Char)]]
            getNums [] = []
            getNums xs =  takeWhile (isDigit . snd) xs : getNums (dropWhile (not . isDigit . snd) $ dropWhile (isDigit . snd) xs)
            toNum :: [(Point,Char)] -> Number
            toNum xs = let  ((l,y),_) = head xs
                            ((r,_),_) = last xs
                        in (y,l,r)
            getNumsLine :: Int -> [Number]
            getNumsLine y = map toNum $ getNums $ dropWhile (not . isDigit . snd) [ ((x,y), field ! (x,y)) | x <- [xl..xr] ]
        return $ concatMap getNumsLine [yl..yr]

validPoint :: Point -> R Bool
validPoint p = reader $ \field -> A.inRange (A.bounds field) p

getNeighbours :: Number -> R [Char]
getNeighbours (y,xleft,xright) = do
        points <- filterM validPoint ((xleft-1,y):(xright+1,y):[(x,yn) | x<-[(xleft-1)..(xright+1)], yn<-[y-1,y+1] ])
        field <- ask
        return $ map (field !) points

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && (c /= '.')

isPart :: Number -> R Bool
isPart number = getNeighbours number >>= return . any isSymbol