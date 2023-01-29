-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit
import Foreign (Bits(testBit))
import Data.Either (Either(Right))

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Tests: " [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ show p
  ,
  testGroup "num Tests"[
  testCase "114514" $
    parseString "114514" @?=
      Right [SExp (Const (IntVal 114514))],
  testCase "-114514" $
    parseString "-114514" @?=
      Right [SExp (Const (IntVal (-114514)))],
  testCase "007" $
    case parseString "007" of
      Left e -> return ()
      Right p -> assertFailure $ show p,
  testCase "-0" $
     parseString "-0" @?=
      Right [SExp (Const (IntVal 0))]
  ],
  testGroup "String Tests" [
    testCase "qwer" $
      parseString  "'qwer'" @?=
        Right [SExp (Const (StringVal "qwer"))],
    testCase "qwe\nr" $
      parseString  "'qwe\\nr'" @?=
        Right [SExp (Const (StringVal "qwe\nr"))],
    testCase "qwe\'\nr" $
      parseString  "'qwe\\'\\nr'" @?=
        Right [SExp (Const (StringVal "qwe'\nr"))],
    testCase "\\nqwer" $
      parseString  "'\\\nqwer'" @?=
        Right [SExp (Const (StringVal "qwer"))]
  ],
  testGroup "ident Tests" [
    testCase "qwer" $
      parseString "qwer" @?=
        Right [SExp (Var "qwer")],
    testCase "qweqwe123123" $
      parseString "qweqwe123123" @?=
        Right [SExp (Var "qweqwe123123")],
    testCase "__qwer" $
      parseString "__qwer" @?=
        Right [SExp (Var "__qwer")],
    testCase "if" $
       case parseString "if" of
          Left e -> return ()  
          Right p -> assertFailure $ show p
          ,
    testCase "in" $
        case parseString "in" of
          Left e -> return ()  
          Right p -> assertFailure $ show p
  ],
  testGroup "Expr Tests" [
    testCase "False" $
      parseString "False" @?=
        Right [SExp (Const FalseVal)],
    testCase "not" $
      parseString "not True" @?=
        Right [SExp (Not (Const TrueVal))],
    testCase "(Expr)" $
      parseString "not (True)" @?=
        Right [SExp (Not (Const TrueVal))],
    testCase "range (1,5,2)" $
      parseString "range (1,5,2)" @?=
        Right [SExp (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 2)])],
    testCase "[13,938,222]" $
      parseString "[13,938,222]" @?=
        Right [SExp (List [Const (IntVal 13),Const (IntVal 938),Const (IntVal 222)])],
    testCase "[i for i in range(1,5,1)]" $
      parseString "[i for i in range(1,5,1)]" @?=
        Right [SExp (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)])])],
    testCase "[i for i in range(1,5,1) if i > 2]" $
      parseString "[i for i in range(1,5,1) if i > 2]" @?=
        Right [SExp (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 5),Const (IntVal 1)]),CCIf (Oper Greater (Var "i") (Const (IntVal 2)))])],
    testCase "1+1" $
      parseString "1 + 1" @?=
        Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))],
    testCase "1-1" $
      parseString "1 - 1" @?=
        Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 1)))],
    testCase "1/1" $
      parseString "1 // 1" @?=
        Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 1)))],
    testCase "1%1" $
      parseString "1 % 1" @?=
        Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal 1)))],
    testCase "1==1" $
      parseString "1 == 1" @?=
        Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 1)))],
    testCase "1>1" $
      parseString "1 > 1" @?=
        Right [SExp (Oper Greater (Const (IntVal 1)) (Const (IntVal 1)))],
    testCase "1 <= 1" $
      parseString "1 <= 1" @?=
        Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 1))))],
    testCase "x in x / x" $
      parseString "x in x // x" @?=
        Right [SExp (Oper In (Var "x") (Oper Div (Var "x") (Var "x")))],
    testCase "1*2+3" $
      parseString "1*2+3" @?=
        Right [SExp (Oper Plus (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
    testCase "1+2*3" $
      parseString "1+2*3" @?=
        Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
    testCase "1*2+3*4" $
      parseString "1*2+3*4" @?=
        Right [SExp (Oper Plus (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Oper Times (Const (IntVal 3)) (Const (IntVal 4))))]
    ],
  testGroup "Stmt Tests" [
    testCase "a = 1" $
      parseString "a = 1" @?=
        Right [SDef "a" (Const (IntVal 1))],
    testCase "x= for i in range(1,2,3)" $
      parseString "x= [i for i in range(1,2,3)]" @?=
        Right [SDef "x" (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])])]
  ],
  testGroup "Stmts Tests" [
    testCase "x=[i for i in range(1,2,3)];print(x)" $
      parseString "x=[i for i in range(1,2,3)];print(x)" @?=
        Right [SDef "x" (Compr (Var "i") [CCFor "i" (Call "range" [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)])]),SExp (Call "print" [Var "x"])]
    ],
  testGroup "Comment Tests"[
    testCase "x=123#qweqweqwe\nprint(x)" $
      parseString "x=123;#qweqweqwe\nprint(x)" @?=
      Right [SDef "x" (Const (IntVal 123)),SExp (Call "print" [Var "x"])],
    testCase "#Comments\n3" $
      parseString "#Comments\n3" @?=
        Right [SExp (Const (IntVal 3))]
  ]
      ]