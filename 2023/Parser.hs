module Parser (
    Parser (..),
    parseResult,
    parseResultMaybe,
    satisfy,
    anyChar,
    char,
    string,
    digit,
    int,
    onezero,
    signedInt,
    space,
    newline,
    oneOfChar,
    oneOfString,
    between,
    inBrackets,
    separatedBy,
    commaSep,
    word
) where

import Data.Char (isDigit, digitToInt, isAlpha)
import Data.Maybe (fromJust, maybeToList)
import Control.Applicative (Alternative (..), asum)

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
    (x:xs)  -> if p x then (xs, Just x) else (x:xs, Nothing)

anyChar :: Parser Char
anyChar = satisfy (const True)

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string s = traverse char s

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

int :: (Read a, Integral a) => Parser a
int = read <$> some (satisfy isDigit)

onezero :: Parser a -> Parser [a]
onezero (P p) = P $ \input -> case p input of
    (rest, Just a) -> (rest, Just [a])
    (rest, Nothing) -> (rest, Just [])

signedInt :: (Read a, Integral a) => Parser a
signedInt = read <$> ( (++) <$> onezero (char '-') <*> some (satisfy isDigit) )

space :: Parser ()
space = char ' ' *> pure ()

newline :: Parser ()
newline = char '\n' *> pure ()

oneOfString :: [String] -> Parser String
oneOfString xs = asum $ map string xs

oneOfChar :: [Char] -> Parser Char
oneOfChar xs = asum $ map char xs

between :: Parser a -> Parser c -> Parser b -> Parser b
between left right within = left *> within <* right

inBrackets :: Parser a -> Parser a
inBrackets = between (char '{') (char '}')

separatedBy :: Parser a -> Parser b -> Parser [b]
separatedBy comma ding = let dingcomma = ding <* comma
                         in (\ds d -> ds ++ [d]) <$> many dingcomma <*> ding

commaSep :: Parser b -> Parser [b]
commaSep = separatedBy (char ',')

word :: Parser String
word = many (satisfy isAlpha)