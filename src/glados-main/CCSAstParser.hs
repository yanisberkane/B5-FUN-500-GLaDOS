module CCSAstParser (
    -- *CCS to Ast representation
    -- $ccsastparser
    parseCCSAst,
    bufferToCCSAst,

    -- *ParserAst
    -- $parserast
    parseAstInt,
    parseAstBool,
    parseAstSymbol,
    parseAstString,
    parseSeparator,
    parseAstOperator,
    parseAstLogicOperator,
    parseDefine,
    parseAssign,
    parseAstList,
    parseBody,
    parseArgList,
    parseIf,
    parseLambda,
    parseNamedFunc,
    parseCall,
    parseMathOperation
)where
import Types
import Parser
import Control.Applicative
import Data.Maybe
import qualified Data.Functor

{- $ccsastparser
    This module contains the parser used in the Glados interpreter.
    It takes a string and returns a list of Asts.

    "Parser" is the type used to represent the parser.
-}

{- $parserast
    This module contains the functions used to parse the Glados language.
-}

parseAstInt :: Parser Ast -- ^ parseAstInt parses an AstInt and returns it.
parseAstInt = AstInt <$> (parseMany parseWhiteSpace *> (parseInt <|> parseUInt))

parseAstBool :: Parser Ast -- ^ parseAstBool parses an AstBool and returns it.
parseAstBool = AstBool <$> (parseMany parseWhiteSpace *> parseBool)

parseAstSymbol :: Parser Ast -- ^ parseAstSymbol parses an AstSymbol and returns it.
parseAstSymbol = AstSymbol <$> (parseMany parseWhiteSpace *> parseSymbol)

parseAstString :: Parser Ast -- ^ parseAstString parses an AstString and returns it.
parseAstString = AstString <$> (parseMany parseWhiteSpace *> parseQuotedSymbol)

parseSeparator :: Parser Ast -- ^ parseSeparator parses a Separator and returns it.
parseSeparator = Separator <$> (parseMany parseWhiteSpace *> parseChar ';')

parseAstOperator :: Parser Ast -- ^ parseAstOperator parses an AstOperator and returns it.
parseAstOperator = AstOperator <$>
    (parseMany parseWhiteSpace *> parseOperator)

parseAstLogicOperator :: Parser Ast -- ^ parseAstLogicOperator parses a LogicOperator and returns it.
parseAstLogicOperator = LogicOperator <$>
    (parseMany parseWhiteSpace *> parseLogicOperator)

parseDefine :: Parser Ast -- ^ parseDefine parses a Define and returns it.
parseDefine = parseMany parseWhiteSpace *>
    parseOr (parseString "let") (parseString "var")
    *> parseAstSymbol >>= \varName -> parseMany parseWhiteSpace *>
    parseChar '=' *> parseCCSAst
    >>= \value -> parseMany parseWhiteSpace <* parseSeparator
    Data.Functor.$> Define varName value

parseAssign :: Parser Ast -- ^ parseAssign parses an Assign and returns it.
parseAssign = parseAstSymbol >>= \varName ->
    parseMany parseWhiteSpace *> parseChar '='
    *> parseCCSAst >>= \value -> parseSeparator
    Data.Functor.$> Assign varName value

parseAstList :: Parser Ast -- ^ parseAstList parses an AstList and returns it.
parseAstList = parseMany parseWhiteSpace *> parseChar '('
    *> parseMany parseCCSAst >>= \args -> parseMany parseWhiteSpace <* parseChar ')'
    Data.Functor.$> AstList args

parseBody :: Parser Ast -- ^ parseBody parses an AstList and returns it.
parseBody = parseMany parseWhiteSpace *> parseChar '{'
    *> parseMany parseCCSAst >>= \args -> parseMany parseWhiteSpace <* parseChar '}'
    Data.Functor.$> AstList args

parseArgList :: Parser Ast -- ^ parseArgList parses an AstList and returns it.
parseArgList = parseMany parseWhiteSpace *> parseChar '('
    *> parseMany (parseOr (parseCCSAst <* parseSome (parseChar ',')) parseCCSAst)
    >>= \args -> parseMany parseWhiteSpace <* parseChar ')'
    Data.Functor.$> AstList args

parseIf :: Parser Ast -- ^ parseIf parses an If and returns it.
parseIf = parseOr (parseMany parseWhiteSpace *> parseString "if" *> parseAstList >>=
    \cond -> parseMany parseWhiteSpace *> parseString "then:" *> parseCCSAst >>= \thenExpr ->
    parseMany parseWhiteSpace *> parseOr
    (parseString "else:" *> parseCCSAst >>= \elseExpr -> parseMany parseWhiteSpace Data.Functor.$> If cond thenExpr elseExpr)
    (parseMany parseWhiteSpace Data.Functor.$> If cond thenExpr AstNone))
    (parseAstList >>= \cond -> parseMany parseWhiteSpace *> parseChar '?' *> parseCCSAst >>= \thenExpr ->
    parseMany parseWhiteSpace *> parseChar ':' *> parseCCSAst >>= \elseExpr ->
    parseMany parseWhiteSpace Data.Functor.$> If cond thenExpr elseExpr)

parseLambda :: Parser Ast -- ^ parseLambda parses a Lambda and returns it.
parseLambda = parseArgList >>= \params ->
    parseMany parseWhiteSpace *> parseString "=>" *>
    parseOr parseAstList parseBody
    >>= \body -> parseMany parseWhiteSpace
    Data.Functor.$> Lambda params body

parseNamedFunc :: Parser Ast -- ^ parseNamedFunc parses a NamedCall and returns it.
parseNamedFunc = parseAstSymbol >>= \name ->
    parseLambda >>= \lambda -> parseSeparator
    Data.Functor.$> NamedCall name lambda

parseCall :: Parser Ast -- ^ parseCall parses a AstCall and returns it.
parseCall = parseAstSymbol >>= \name ->
    parseArgList >>= \args -> parseMany parseSeparator
    Data.Functor.$> AstCall name args

parseMathOperation :: Parser Ast -- ^ parseMathOperation parses a AstMathOp and returns it.
parseMathOperation = parseMany parseWhiteSpace
    *> parseOr parseCall (parseOr parseAstList (parseOr parseAstInt parseAstSymbol))  >>= \arg1 -> parseMany parseWhiteSpace
    *> parseOperator >>= \op -> parseMany parseWhiteSpace
    *> parseOr parseCall (parseOr parseAstList (parseOr parseAstInt parseAstSymbol)) >>= \arg2 -> parseMany parseWhiteSpace
    Data.Functor.$> AstMathOp arg1 (AstOperator op) arg2


parseCCSAst :: Parser Ast -- ^ parseCCSAst parses an Ast from CCS Syntax and returns it.
parseCCSAst = parseAstLogicOperator
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
          <|> parseAstOperator
          <|> parseAstString
          <|> parseAstSymbol

bufferToCCSAst :: String -> Maybe [Ast] -- ^ bufferToCCSAst parses a string and returns a list of Asts.
bufferToCCSAst buffer = case runParser (parseMany parseCCSAst) buffer of
    Just (x, _) -> Just x