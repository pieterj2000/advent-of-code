import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either (fromRight)
import Data.Char (digitToInt)
import Data.Void (Void)

inputFile = "3a.input"

main = do
    input <- readFile inputFile
    print $ sum . filterdings . fromRight [] . parse parser "" $ input

data Ding = Do | Dont | Int Int deriving (Show, Read, Eq)

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

ding :: Parser Ding
ding = (Do <$ string "do()") <|> (Dont <$ string "don't()")

mulording :: Parser Ding
mulording = try (Int <$> mul) <|> try ding <|> try (anySingle *> mulording)

parser :: Parser [Ding]
parser = many mulording

filterdings :: [Ding] -> [Int]
filterdings [] = []
filterdings (Dont:xs) = filterdings (dropWhile (/= Do) xs)
filterdings (Do:xs) = filterdings xs
filterdings (Int x:xs) = x : filterdings xs