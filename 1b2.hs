-- stack script --resolver lts-21.22
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf,stripPrefix)
import Data.Maybe (isJust, fromJust)
import Control.Applicative (Alternative (..), asum)

input = "1a.input"

main = do
    input <- readFile input
    print $ calc input


calc :: String -> Int
calc = sum . map calcLine . lines

newtype Parser a = P { parse :: String -> (String, Maybe a) }
parseResultMaybe :: Parser a -> String -> Maybe a
parseResultMaybe p = snd . parse p
parseResult :: Parser a -> String -> a
parseResult p = fromJust . snd . parse p

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (P ps) = P $ \input -> let (rest, a) = ps input in (rest, f <$> a)

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P $ \input -> (input, Just a)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (P ps) <*> parser = P $ \input -> case ps input of
        (rest, Nothing) -> (rest, Nothing)
        (rest, Just f)  -> parse (f <$> parser) rest

instance Alternative Parser where
    empty :: Parser a
    empty = P $ \input -> (input, Nothing)
    (<|>) :: Parser a -> Parser a -> Parser a
    (P psA) <|> (P psB) = P $ \input -> case psA input of
        (rest, Just a) -> (rest, Just a)
        (rest, Nothing) -> psB input

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \input -> case input of
    []      -> ([], Nothing)
    (x:xs)  -> if p x then (xs, Just x) else (input, Nothing)

anyChar :: Parser Char
anyChar = satisfy (const True)

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string s = traverse char s

digit :: Parser Int
digit = read . (:[]) <$> satisfy isDigit

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
rdigits = map reverse digits

digitText :: Parser Int
digitText = let p s i = string s *> pure i
            in asum (zipWith p digits [1..])

rdigitText :: Parser Int
rdigitText = let p s i = string s *> pure i
            in asum (zipWith p rdigits [1..])

digitSequence :: Parser [Int]
digitSequence = let p = digitText <|> digit <|> (anyChar *> p)
                in some p

rdigitSequence :: Parser [Int]
rdigitSequence = let p = rdigitText <|> digit <|> (anyChar *> p)
                in some p

calcLine :: String -> Int
calcLine line = let l = head $ parseResult digitSequence line
                    r = head $ parseResult rdigitSequence $ reverse line
                in l*10+r