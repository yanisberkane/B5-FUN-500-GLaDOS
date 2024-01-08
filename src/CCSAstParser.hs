import Types
import Parser
import Control.Applicative
import Data.Maybe
import qualified Data.Functor

parseAstInt :: Parser Ast
parseAstInt = AstInt <$> (parseMany parseWhiteSpace *> (parseInt <|> parseUInt))

parseAstBool :: Parser Ast
parseAstBool = AstBool <$> (parseMany parseWhiteSpace *> parseBool)

parseAstSymbol :: Parser Ast
parseAstSymbol = AstSymbol <$> (parseMany parseWhiteSpace *> parseSymbol)

parseAstString :: Parser Ast
parseAstString = AstString <$> (parseMany parseWhiteSpace *> parseQuotedSymbol)

parseSeparator :: Parser Ast
parseSeparator = Separator <$> (parseMany parseWhiteSpace *> parseChar ';')

parseDefine :: Parser Ast
parseDefine = parseMany parseWhiteSpace *> parseString "define" *> parseMany parseWhiteSpace
    *> parseSymbol >>= \varName -> parseMany parseWhiteSpace *>
    parseChar '=' *> parseMany parseWhiteSpace *> parseCCSAst >>= \value -> parseMany parseWhiteSpace
    Data.Functor.$> Define (AstSymbol varName) value <* parseSeparator

parseList :: Parser Ast
parseList = parseMany parseWhiteSpace *> parseChar '(' *> parseMany parseWhiteSpace
    *> parseMany parseCCSAst >>= \args -> parseMany parseWhiteSpace
    Data.Functor.$> AstList args <* parseChar ')'

parseIf :: Parser Ast
parseIf = parseMany parseWhiteSpace *> parseString "if" *> parseMany parseWhiteSpace
    *> parseCCSAst >>= \cond -> parseMany parseWhiteSpace *> parseString "then:" *> parseSome parseWhiteSpace
    *> parseCCSAst >>= \thenExpr -> parseMany parseWhiteSpace *> parseString "else:" *> parseSome parseWhiteSpace
    *> parseCCSAst >>= \elseExpr -> parseMany parseWhiteSpace
    Data.Functor.$> If cond thenExpr elseExpr

parseLambda :: Parser Ast
parseLambda = parseMany parseWhiteSpace *> parseList >>= \params ->
    parseSome parseWhiteSpace *> parseString "=>" *> parseSome parseWhiteSpace *>
    parseList >>= \body -> parseMany parseWhiteSpace <* parseSeparator
    Data.Functor.$> Lambda params body

parseNamedFunc :: Parser Ast
parseNamedFunc = parseMany parseWhiteSpace *> parseSymbol >>= \name ->
    parseMany parseWhiteSpace *> parseChar '(' *> parseMany parseWhiteSpace
    *> parseMany parseCCSAst >>= \args -> parseMany parseWhiteSpace <* parseChar ')' <* parseSeparator
    Data.Functor.$> NamedFunc (AstSymbol name) (AstList args) 

parseCCSAst :: Parser Ast
parseCCSAst = parseAstInt
          <|> parseAstBool
          <|> parseAstString
          <|> parseIf
          <|> parseDefine
          <|> parseLambda
          <|> parseNamedFunc
          <|> parseList
          <|> parseAstSymbol

bufferToCCSAst :: String -> Maybe [Ast]
bufferToCCSAst buffer = case runParser (parseMany parseCCSAst) buffer of
    Just (x, _) -> Just x
    Nothing -> Nothing