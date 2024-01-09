module CCSAstParser (
    bufferToCCSAst
) where
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
parseDefine = parseMany parseWhiteSpace *> parseString "let" *> parseMany parseWhiteSpace
    *> parseAstSymbol >>= \varName -> parseMany parseWhiteSpace *>
    parseChar '=' *> parseMany parseWhiteSpace *> parseCCSAstExceptSymbol >>= \value -> parseMany parseWhiteSpace
    Data.Functor.$> Define varName value <* parseSeparator

parseAssign :: Parser Ast
parseAssign = parseMany parseWhiteSpace *> parseAstSymbol >>= \varName ->
    parseMany parseWhiteSpace *> parseChar '=' *> parseMany parseWhiteSpace
    *> parseCCSAst >>= \value -> parseMany parseWhiteSpace <* parseSeparator
    Data.Functor.$> Assign varName value

parseAstList :: Parser Ast
parseAstList = parseMany parseWhiteSpace *> parseChar '(' *> parseMany parseWhiteSpace
    *> parseMany parseCCSAst >>= \args -> parseMany parseWhiteSpace
    Data.Functor.$> AstList args <* parseChar ')'

parseIf :: Parser Ast
parseIf = parseMany parseWhiteSpace *> parseString "if" *> parseMany parseWhiteSpace
    *> parseCCSAst >>= \cond -> parseMany parseWhiteSpace *> parseString "then:" *> parseSome parseWhiteSpace
    *> parseCCSAst >>= \thenExpr -> parseMany parseWhiteSpace *>
    parseOr
    (parseString "else:" *> parseSome parseWhiteSpace *> parseCCSAst >>= \elseExpr -> parseMany parseWhiteSpace
    Data.Functor.$> If cond thenExpr elseExpr)
    (parseMany parseWhiteSpace Data.Functor.$> If cond thenExpr AstNone)

parseLambda :: Parser Ast
parseLambda = parseMany parseWhiteSpace *> parseAstList >>= \params ->
    parseSome parseWhiteSpace *> parseString "=>" *> parseSome parseWhiteSpace *>
    parseAstList >>= \body -> parseMany parseWhiteSpace
    Data.Functor.$> Lambda params body

parseNamedFunc :: Parser Ast
parseNamedFunc = parseMany parseWhiteSpace *> parseAstSymbol >>= \name ->
    parseLambda >>= \lambda -> parseMany parseWhiteSpace <* parseSeparator
    Data.Functor.$> NamedCall name lambda

parseCall :: Parser Ast
parseCall = parseMany parseWhiteSpace *> parseAstSymbol >>= \name ->
    parseMany parseWhiteSpace *> parseAstList >>= \args -> parseMany parseWhiteSpace <* parseSeparator
    Data.Functor.$> AstCall name args

parseCCSAst :: Parser Ast
parseCCSAst = parseIf
          <|> parseDefine
          <|> parseAssign
          <|> parseNamedFunc
          <|> parseLambda
          <|> parseCall
          <|> parseAstBool
          <|> parseAstInt
          <|> parseAstString
          <|> parseAstList
          <|> parseAstSymbol

parseCCSAstExceptSymbol :: Parser Ast
parseCCSAstExceptSymbol = parseIf
          <|> parseDefine
          <|> parseAssign
          <|> parseNamedFunc
          <|> parseLambda
          <|> parseCall
          <|> parseAstBool
          <|> parseAstInt
          <|> parseAstString
          <|> parseAstList

bufferToCCSAst :: String -> Maybe [Ast]
bufferToCCSAst buffer = case runParser (parseMany parseCCSAst) buffer of
    Just (x, _) -> Just x
    Nothing -> Nothing