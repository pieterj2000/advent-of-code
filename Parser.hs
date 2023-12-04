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
    space
) where

import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromJust)
import Control.Applicative (Alternative (..))

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
    (x:xs)  -> if p x then (xs, Just x) else (xs, Nothing)

anyChar :: Parser Char
anyChar = satisfy (const True)

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string s = traverse char s

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

int :: Parser Int
int = read <$> some (satisfy isDigit)

space :: Parser ()
space = char ' ' *> pure ()