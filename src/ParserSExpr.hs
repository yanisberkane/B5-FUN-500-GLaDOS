
module ParserSExpr (
    stringToSExpr
) where
import Parser
import Types
import Control.Applicative
import Data.Maybe

parseSSymbol :: Parser SExpr
parseSSymbol = SSymbol <$> parseOr (parseMany parseWhiteSpace *> parseSymbol) (parseMany parseWhiteSpace *> parseQuotedSymbol)

parseSInt :: Parser SExpr
parseSInt = SInt <$> (parseMany parseWhiteSpace *> (parseInt <|> parseUInt))

parseSList :: Parser SExpr
parseSList = fmap SList $ parseMany parseWhiteSpace *> parseChar '(' *> parseMany (parseMany parseWhiteSpace *> parseExpr <* parseMany parseWhiteSpace) <* parseChar ')'

parseLambda :: Parser SExpr
parseLambda = do
    parseMany parseWhiteSpace
    parseChar '('
    parseMany parseWhiteSpace
    parseString "lambda"
    parseMany parseWhiteSpace
    params <- parseChar '(' *> parseMany (parseMany parseWhiteSpace *> parseSymbol <* parseMany parseWhiteSpace) <* parseChar ')'
    body <- parseMany parseWhiteSpace *> parseExpr
    parseMany parseWhiteSpace
    parseChar ')'
    let astBody = fromJust $ sexprToAst body
    return $ SList [SSymbol "lambda", SList (map SSymbol params), astToSExpr astBody]

parseIf :: Parser SExpr
parseIf = do
    parseMany parseWhiteSpace
    parseChar '('
    parseMany parseWhiteSpace
    parseString "if"
    condition <- parseMany parseWhiteSpace *> parseExpr
    thenBranch <- parseMany parseWhiteSpace *> parseExpr
    elseBranch <- parseMany parseWhiteSpace *> parseExpr
    parseMany parseWhiteSpace
    parseChar ')'
    let astCond = fromJust $ sexprToAst condition
    let astThen = fromJust $ sexprToAst thenBranch
    let astElse = fromJust $ sexprToAst elseBranch
    return $ SList [SSymbol "if", astToSExpr astCond, astToSExpr astThen, astToSExpr astElse]

parseNamedFunction :: Parser SExpr
parseNamedFunction = do
    parseMany parseWhiteSpace
    parseChar '('
    parseMany parseWhiteSpace
    parseString "define"
    parseMany parseWhiteSpace
    parseChar '('
    funcName <- parseSymbol
    parseMany parseWhiteSpace
    params <- parseMany (parseMany parseWhiteSpace *> parseSymbol <* parseMany parseWhiteSpace)
    parseMany parseWhiteSpace
    parseChar ')'
    body <- parseMany parseWhiteSpace *> parseExpr
    parseMany parseWhiteSpace
    parseChar ')'
    let astLambda = Lambda params (fromJust $ sexprToAst body)
    return $ SList [SSymbol "define", SList (SSymbol funcName : map SSymbol params), astToSExpr astLambda]

parseSBool :: Parser SExpr
parseSBool = (parseChar '#' *> (parseChar 't' *> pure (SBool True) <|> parseChar 'f' *> pure (SBool False)))

parseSString :: Parser SExpr
parseSString = SString <$> parseQuotedString

parseQuotedString :: Parser String
parseQuotedString = do
    parseChar '"'
    str <- parseMany (parseNoneOf "\"")
    parseChar '"'
    return str

parseExpr :: Parser SExpr
parseExpr = parseSString
         <|> parseSInt
         <|> parseSSymbol
         <|> parseSList
         <|> parseLambda
         <|> parseIf
         <|> parseNamedFunction
         <|> parseSBool

stringToSExpr :: String -> Maybe [SExpr]
stringToSExpr input = case runParser (parseMany parseExpr) input of
    Just (x, _) -> Just x
    Nothing -> Nothing