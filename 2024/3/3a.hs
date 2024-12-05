import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either (fromRight)
import Data.Char (digitToInt)
import Data.Void (Void)

inputFile = "3a.input"

main = do
    input <- readFile inputFile
    print $ sum . fromRight [] . parse parser "" $ input


type Parser = Parsec Void String
nummer :: Parser Int
nummer = maakgetal <$> (Just <$> digitChar) <*> (optional digitChar) <*> (optional digitChar)
    where
        maakgetal :: Maybe Char -> Maybe Char -> Maybe Char -> Int
        maakgetal (Just a) Nothing _ = digitToInt a
        maakgetal (Just a) (Just b) Nothing = 10*(digitToInt a) + (digitToInt b)
        maakgetal (Just a) (Just b) (Just c) =  100*(digitToInt a) + 10*(digitToInt b) + (digitToInt c)

mul :: Parser Int
mul = (*) <$> (string "mul(" *> nummer) <*> (char ',' *> nummer <* single ')')

mul' :: Parser Int
mul' = try mul <|> try (anySingle *> mul')

parser :: Parser [Int]
parser = many mul'