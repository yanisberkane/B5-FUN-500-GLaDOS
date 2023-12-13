import Parser
import Types

-- fromStrToSList :: String -> Maybe SExpr
-- fromStrToSList str = case runParser parseList str of
--     Just (x, _) -> Just $ SList x
--     Nothing -> Nothing

fromStrToSList :: String -> Maybe SExpr
fromStrToSList str = case runParser parseList str of
    Just (x, _) -> Just $ SList x
    Nothing -> Nothing

fromStrToSSymbol :: String -> Maybe SExpr
fromStrToSSymbol str = case runParser parseSymbol str of
    Just (x, _) -> Just $ SSymbol x
    Nothing -> Nothing

fromStrToSInt :: String -> Maybe SExpr
fromStrToSInt nbr = case runParser (parseOr parseInt parseUInt) nbr of
    Just (x, _) -> Just $ SInt x
    Nothing -> Nothing

-- fromStrToSExpr :: String -> Maybe SExpr
-- fromStrToSExpr str = fromStrToSSymbol str | (fromStrToSInt str)
    -- Nothing -> Nothing
-- intToSExpr nbr = Just SInt
-- intToSExpr _ = Nothing