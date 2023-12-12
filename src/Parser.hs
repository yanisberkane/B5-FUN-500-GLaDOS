import System.IO
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T

data Parser a = Parser {
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
    empty = Parser $ \_ -> Nothing
    p1 <|> p2 = Parser $ \str -> case runParser p1 str of
        Just (x, xs) -> Just (x, xs)
        Nothing -> runParser p2 str

instance Monad Parser where
    return = pure
    p >>= fct = Parser $ \str -> case runParser p str of
        Just (x, xs) -> runParser (fct x) xs
        Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \str -> case str of
    (x:xs) | c == x -> Just (c, xs)
    _ -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ \str' -> case str' of
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
        Just (y, ys) -> Just ((f x y), ys)
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

parseUInt :: Parser Int
parseUInt = fmap read $ parseSome (parseAnyChar "0123456789")

parseInt :: Parser Int
parseInt = (\x y -> if x == '-' then -y else y) <$> parseOr (parseChar '-') (parseChar '+') <*> parseUInt

parseWhiteSpace :: Parser String
parseWhiteSpace = parseMany $ parseAnyChar " \t\n"

-- runParser parsePair parseInt "(123 456) foo bar"
-- Just ((123,456)," foo bar")
-- parsePair :: Parser a -> Parser (a, a)
-- parsePair p = parseAndWith (,) (parseChar '(' *> parseWhiteSpace *> p) (parseWhiteSpace *> parseChar ')')

parseInput :: String -> IO ()
parseInput filename = do
    fileHandle <- openFile filename ReadMode
    contents <- hGetContents fileHandle
    print contents
    hClose fileHandle
