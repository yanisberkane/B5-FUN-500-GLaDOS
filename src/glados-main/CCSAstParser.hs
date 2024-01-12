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

parseAstOperator :: Parser Ast
parseAstOperator = AstOperator <$>
    (parseMany parseWhiteSpace *> parseOperator)

parseAstLogicOperator :: Parser Ast
parseAstLogicOperator = LogicOperator <$>
    (parseMany parseWhiteSpace *> parseLogicOperator)

parseDefine :: Parser Ast
parseDefine = parseMany parseWhiteSpace *>
    parseOr (parseString "let") (parseString "var")
    *> parseAstSymbol >>= \varName -> parseMany parseWhiteSpace *>
    parseChar '=' *> parseCCSAst
    >>= \value -> parseMany parseWhiteSpace <* parseSeparator
    Data.Functor.$> Define varName value

parseAssign :: Parser Ast
parseAssign = parseAstSymbol >>= \varName ->
    parseMany parseWhiteSpace *> parseChar '='
    *> parseCCSAst >>= \value -> parseSeparator
    Data.Functor.$> Assign varName value

parseAstList :: Parser Ast
parseAstList = parseMany parseWhiteSpace *> parseChar '('
    *> parseMany parseCCSAst >>= \args -> parseMany parseWhiteSpace <* parseChar ')'
    Data.Functor.$> AstList args

parseBody :: Parser Ast
parseBody = parseMany parseWhiteSpace *> parseChar '{'
    *> parseMany parseCCSAst >>= \args -> parseMany parseWhiteSpace <* parseChar '}'
    Data.Functor.$> AstList args

parseArgList :: Parser Ast
parseArgList = parseMany parseWhiteSpace *> parseChar '('
    *> parseMany (parseOr (parseCCSAst <* parseSome (parseChar ',')) parseCCSAst)
    >>= \args -> parseMany parseWhiteSpace <* parseChar ')'
    Data.Functor.$> AstList args

parseIf :: Parser Ast
parseIf = parseOr (parseMany parseWhiteSpace *> parseString "if" *> parseAstList >>=
    \cond -> parseMany parseWhiteSpace *> parseString "then:" *> parseCCSAst >>= \thenExpr ->
    parseMany parseWhiteSpace *> parseOr
    (parseString "else:" *> parseCCSAst >>= \elseExpr -> parseMany parseWhiteSpace Data.Functor.$> If cond thenExpr elseExpr)
    (parseMany parseWhiteSpace Data.Functor.$> If cond thenExpr AstNone))
    (parseAstList >>= \cond -> parseMany parseWhiteSpace *> parseChar '?' *> parseCCSAst >>= \thenExpr ->
    parseMany parseWhiteSpace *> parseChar ':' *> parseCCSAst >>= \elseExpr ->
    parseMany parseWhiteSpace Data.Functor.$> If cond thenExpr elseExpr)

parseLambda :: Parser Ast
parseLambda = parseArgList >>= \params ->
    parseMany parseWhiteSpace *> parseString "=>" *>
    parseOr parseAstList parseBody
    >>= \body -> parseMany parseWhiteSpace
    Data.Functor.$> Lambda params body

parseNamedFunc :: Parser Ast
parseNamedFunc = parseAstSymbol >>= \name ->
    parseLambda >>= \lambda -> parseSeparator
    Data.Functor.$> NamedCall name lambda

parseCall :: Parser Ast
parseCall = parseAstSymbol >>= \name ->
    parseArgList >>= \args -> parseMany parseSeparator
    Data.Functor.$> AstCall name args

parseMathOperation :: Parser Ast
parseMathOperation = parseMany parseWhiteSpace
    *> parseOr parseAstList (parseOr parseAstSymbol parseAstInt)  >>= \arg1 -> parseMany parseWhiteSpace
    *> parseOperator >>= \op -> parseMany parseWhiteSpace
    *> parseOr parseAstList (parseOr parseAstSymbol parseAstInt) >>= \arg2 -> parseMany parseWhiteSpace
    Data.Functor.$> AstMathOp arg1 (AstOperator op) arg2

parseCCSAst :: Parser Ast
parseCCSAst = parseAstLogicOperator
          <|> parseAstOperator
          <|> parseMathOperation
          <|> parseIf
          <|> parseDefine
          <|> parseAssign
          <|> parseNamedFunc
          <|> parseLambda
          <|> parseCall
          <|> parseBody
          <|> parseAstList
          <|> parseAstBool
          <|> parseAstInt
          <|> parseAstString
          <|> parseAstSymbol

bufferToCCSAst :: String -> Maybe [Ast]
bufferToCCSAst buffer = case runParser (parseMany parseCCSAst) buffer of
    Just (x, _) -> Just x
    Nothing -> Nothing