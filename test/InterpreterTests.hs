module InterpreterTests where
import Interpreter
import Test.HUnit ( assertEqual, Test(TestCase) )
import Types
import VMTypes

-- testinterpretAST :: Test
-- testinterpretAST = TestCase $ do
--     let expected = ([],[Push (IntValue 1),Push (IntValue 2),PushVMEnv "add",Call 2,AssignEnvValue "foo",Push (IntValue 1),Push (IntValue 2),PushVMEnv "sub",Call 2,AssignEnvValue "bar",Push (IntValue 1),Push (IntValue 2),PushVMEnv "mul",Call 2,AssignEnvValue "baz",Push (IntValue 1),Push (IntValue 2),PushVMEnv "div",Call 2,AssignEnvValue "qux",Ret])
--     assertEqual "interpretAST [Define (AstSymbol \"foo\") (AstCall (AstSymbol \"add\") (AstList [AstInt 1, AstInt 2])), Define (AstSymbol \"bar\") (AstCall (AstSymbol \"sub\") (AstList [AstInt 1, AstInt 2])), Define (AstSymbol \"baz\") (AstCall (AstSymbol \"mul\") (AstList [AstInt 1, AstInt 2])), Define (AstSymbol \"qux\") (AstCall (AstSymbol \"div\") (AstList [AstInt 1, AstInt 2]))]" expected (interpretAST [Define (AstSymbol "foo") (AstCall (AstSymbol "add") (AstList [AstInt 1, AstInt 2])), Define (AstSymbol "bar") (AstCall (AstSymbol "sub") (AstList [AstInt 1, AstInt 2])), Define (AstSymbol "baz") (AstCall (AstSymbol "mul") (AstList [AstInt 1, AstInt 2])), Define (AstSymbol "qux") (AstCall (AstSymbol "div") (AstList [AstInt 1, AstInt 2]))])

-- testInterpretValueInt :: Test
-- testInterpretValueInt = TestCase $ do
--     let expected = [Push (IntValue 1)]
--     assertEqual "interpretValue (AstInt 1)" expected (interpretValue (AstInt 1))

-- testInterpretValueBool :: Test
-- testInterpretValueBool = TestCase $ do
--     let expected = [Push (BoolValue True)]
--     assertEqual "interpretValue (AstBool True)" expected (interpretValue (AstBool True))

-- testInterpretValueString :: Test
-- testInterpretValueString = TestCase $ do
--     let expected = [Push (StringValue "hello")]
--     assertEqual "interpretValue (AstString \"hello\")" expected (interpretValue (AstString "hello"))

-- testInterpretValueSymbol :: Test
-- testInterpretValueSymbol = TestCase $ do
--     let expected = [PushVMEnv "foo"]
--     assertEqual "interpretValue (AstSymbol \"foo\")" expected (interpretValue (AstSymbol "foo"))

-- testInterpretValueError :: Test
-- testInterpretValueError = TestCase $ do
--     let expected = [Push (IntValue 1),Push (IntValue 2),Push (Operator Add),CallOp]
--     assertEqual "interpretValue (AstCall (AstSymbol \"add\") (AstList [AstInt 1, AstInt 2]))" expected (interpretValue (AstCall (AstSymbol "add") (AstList [AstInt 1, AstInt 2])))