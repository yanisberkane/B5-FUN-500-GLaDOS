module CCSParser (
    bufferToCCS
) where
import Parser
import Control.Applicative
import Data.Maybe
import Types (CCS(..))

parseCCSInt :: Parser CCS
parseCCSInt = CCSInt <$> (parseMany parseWhiteSpace *> (parseInt <|> parseUInt))

parseCCSUInt :: Parser CCS
parseCCSUInt = CCSInt <$> (parseMany parseWhiteSpace *> parseUInt)

parseCCSSymbol :: Parser CCS
parseCCSSymbol = CCSSymbol <$> (parseMany parseWhiteSpace *> parseSymbol)

parseCCSChar :: Parser CCS
parseCCSChar = CCSChar <$> (parseMany parseWhiteSpace *> parseChar '\''
    *> parseAnyChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+-=[]{}\\|;:'\",./<>?`~"
    <* parseChar '\'')

parseCCSString :: Parser CCS
parseCCSString = CCSString <$> (parseMany parseWhiteSpace *> parseQuotedSymbol)

parseCCSBool :: Parser CCS
parseCCSBool = CCSBool <$> (parseMany parseWhiteSpace *> parseBool)

parseSeparator :: Parser CCS
parseSeparator = CCSSymbol <$> parseSome (parseOr (parseChar ',') (parseChar ';'))

parseCCSBody :: Parser CCS
parseCCSBody = fmap CCSBody $ parseMany parseWhiteSpace *>
    parseChar '{' *> parseMany
    (parseMany parseWhiteSpace *> parseCCS <* parseMany parseWhiteSpace)
    <* parseChar '}'

parseCCSList :: Parser CCS
parseCCSList = fmap CCSList $ parseMany parseWhiteSpace *>
    parseChar '(' *> parseMany
    (parseMany parseWhiteSpace *> parseCCS <* parseMany parseWhiteSpace)
    <* parseChar ')'

parseCCS :: Parser CCS
parseCCS = parseCCSUInt
            <|> parseCCSInt
            <|> parseCCSChar
            <|> parseCCSString
            <|> parseCCSBool
            <|> parseSeparator
            <|> parseCCSSymbol
            <|> parseCCSBody
            <|> parseCCSList

bufferToCCS :: String -> Maybe [CCS]
bufferToCCS str = case runParser (parseMany parseCCS) str of
    Just (ccs, _) -> Just ccs
    Nothing -> Nothing
