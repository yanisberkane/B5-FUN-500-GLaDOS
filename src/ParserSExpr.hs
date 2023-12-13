
module ParserSExpr (
    stringToSExpr
) where
import Parser
import Types
import Control.Applicative

parseSSymbol :: Parser SExpr
parseSSymbol = SSymbol <$> (parseMany parseWhiteSpace *> parseSymbol)

parseSInt :: Parser SExpr
parseSInt = SInt <$> (parseMany parseWhiteSpace *> (parseInt <|> parseUInt))

parseSList :: Parser SExpr
parseSList = fmap SList $ parseMany parseWhiteSpace *> parseChar '(' *> parseMany (parseMany parseWhiteSpace *> parseExpr <* parseMany parseWhiteSpace) <* parseChar ')'

parseExpr :: Parser SExpr
parseExpr = parseSSymbol
        <|> parseSInt
        <|> parseSList

stringToSExpr :: String -> Maybe SExpr
stringToSExpr input = case runParser parseExpr input of
    Just (x, _) -> Just x
    Nothing -> Nothing