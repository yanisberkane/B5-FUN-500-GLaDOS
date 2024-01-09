module Formatter (formatEnv, formatInsts) where

import VMTypes (Value(..), Instruction(..), VMEnv, Insts)

formatEnv :: VMEnv -> String
formatEnv env = unlines $ map formatBinding env
  where
    formatBinding (var, val) = var ++ " = " ++ showValue val
    showValue (IntValue i) = "INT " ++ show i
    showValue (BoolValue b) = "BOOL " ++ show b
    showValue (StringValue s) = "STRING " ++ s
    showValue (Operator op) = "OPERATOR " ++ show op
    showValue (Function insts) = "FUNCTION " ++ show insts
    showValue (ListValue vals) = "LIST " ++ show vals


formatInsts :: Insts -> String
formatInsts insts = unlines $ map show insts