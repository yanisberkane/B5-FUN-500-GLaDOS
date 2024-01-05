module Types where

import qualified Data.Map as Map
import Data.Maybe

data CCS = CCSInt Int
         | CCSUInt Int
         | CCSSymbol String
         | CCSChar Char
         | CCSString String
         | CCSBool Bool
         | CCSBody [CCS]
         | CCSList [CCS]
         deriving (Show, Eq)

data Ast = AstInt Int
         | AstBool Bool
         | AstSymbol String
         | AstList [Ast]
         | Define Ast Ast Ast
         | AstString String
         | Lambda [String] Ast
         | If Ast Ast Ast
         deriving (Show, Eq)

getCCSSymbol :: CCS -> Maybe String
getCCSSymbol (CCSSymbol str) = Just str
getCCSSymbol _ = Nothing

getSInt :: CCS -> Maybe Int
getSInt (CCSInt int) = Just int
getSInt _ = Nothing

getSList :: CCS -> Maybe [CCS]
getSList (CCSList list) = Just list
getSList _ = Nothing

ccsToAst :: CCS -> Maybe Ast
ccsToAst (CCSList [CCSSymbol "if", cond, thenExpr, elseExpr]) =
    If <$> ccsToAst cond <*> ccsToAst thenExpr <*> ccsToAst elseExpr
ccsToAst (CCSSymbol str) = Just $ AstSymbol str
ccsToAst (CCSString str) = Just $ AstString str
ccsToAst (CCSInt int) = Just $ AstInt int
ccsToAst (CCSBool b) = Just $ AstBool b
ccsToAst (CCSList list) = AstList <$> mapM ccsToAst list

astToCCS :: Ast -> CCS
astToCCS (If cond thenExpr elseExpr) = CCSList [CCSSymbol "if", astToCCS cond, astToCCS thenExpr, astToCCS elseExpr]
astToCCS (AstSymbol str) = CCSSymbol str
astToCCS (AstInt int) = CCSInt int
astToCCS (AstBool b) = CCSBool b
astToCCS (AstList list) = CCSList $ map astToCCS list

type Env = Map.Map String Ast