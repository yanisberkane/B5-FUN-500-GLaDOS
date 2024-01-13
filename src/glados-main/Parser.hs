{-# LANGUAGE LambdaCase #-}
module Parser (
    Parser(..),
    parseChar,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseSymbol,
    parseQuotedSymbol,
    parseWhiteSpace,
    parseString,
    parseBool,
    parseOperator,
    parseLogicOperator,
    parseNoneOf) where
import System.IO
import Data.Maybe
import Control.Applicative
import qualified Data.Functor

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct p = Parser $ \str -> case runParser p str of
        Just (x, xs) -> Just (fct x, xs)
        Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser $ \str -> Just (x, str)
    p1 <*> p2 = Parser $ \str -> case runParser p1 str of
        Just (fct, xs) -> case runParser p2 xs of
            Just (x, ys) -> Just (fct x, ys)
            Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \str -> case runParser p1 str of
        Just (x, xs) -> Just (x, xs)
        Nothing -> runParser p2 str

instance Monad Parser where
    return = pure
    p >>= fct = Parser $ \str -> case runParser p str of
        Just (x, xs) -> runParser (fct x) xs
        Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \case
    (x:xs) | c == x -> Just (c, xs)
    _ -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ \case
    (x:xs) | x `elem` str -> Just (x, xs)
    _ -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser $ \str -> runParser p1 str <|> runParser p2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \str -> case runParser p1 str of
    Just (x, xs) -> case runParser p2 xs of
        Just (y, ys) -> Just ((x, y), ys)
        Nothing -> Nothing
    Nothing -> Nothing

parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \str -> case runParser p1 str of
    Just (x, xs) -> case runParser p2 xs of
        Just (y, ys) -> Just (f x y, ys)
        Nothing -> Nothing
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \str -> case runParser p str of
    Just (x, xs) -> case runParser (parseMany p) xs of
        Just (y, ys) -> Just (x:y, ys)
        Nothing -> Just ([x], xs)
    Nothing -> Just ([], str)

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseNoneOf :: String -> Parser Char
parseNoneOf str = Parser $ \case
    (x:xs) | x `notElem` str -> Just (x, xs)
    _ -> Nothing

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar "0123456789")

parseOperator :: Parser String
parseOperator = parseSome $ parseAnyChar "+-*/%"

parseLogicOperator :: Parser String
parseLogicOperator = parseOr (parseString "&&") (parseString "||")
                 <|> parseOr (parseString "==") (parseString "!=")
                 <|> parseOr (parseString "<=") (parseString ">=")
                 <|> parseOr (parseString "<") (parseString ">")
                 <|> parseString "!"

parseInt :: Parser Int
parseInt = (\x y -> if x == '-' then -y else y) <$> parseOr (parseChar '-') (parseChar '+') <*> parseUInt

parseQuotedSymbol :: Parser String
parseQuotedSymbol = parseChar '\"' *> parseSome (parseNoneOf "\"") <* parseChar '\"'

parseSymbol :: Parser String
parseSymbol = parseSome $ parseOr (parseAnyChar ['a'..'z']) (parseAnyChar ['A'..'Z'])

parseBool :: Parser Bool
parseBool = parseOr (parseString "True"  Data.Functor.$> True)
                    (parseString "False" Data.Functor.$> False)

parseWhiteSpace :: Parser String
parseWhiteSpace = parseSome $ parseAnyChar " \t\n"

parseString :: String -> Parser String
parseString = traverse parseChar
