-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "tests"[
   testGroup "test1"
   [testCase "check x EBadVar env empty" $
    runComp (look "x") [] @?= (Left (EBadVar "x"),[]),
  testCase "check x NoneVal env x" $
    runComp (look "x") [("x",NoneVal)] @?= (Right NoneVal,[]),
  testCase "check x IntVal env x" $
    runComp (look "x") [("x",IntVal 3)] @?= (Right (IntVal 3),[]),
  testCase "check x FalseVal env x" $
    runComp (look "x") [("x",FalseVal)] @?= (Right FalseVal,[]),
  testCase "check x NoneVal" $
    runComp (look "x") [("x",StringVal "text")] @?= (Right (StringVal "text"),[]),
  testCase "check x StringVal env x" $
    runComp (look "x") [("x",ListVal [StringVal "text"])] @?= (Right (ListVal [StringVal "text"]),[]),
  testCase "check x ListVal [StringVal]" $
    runComp (look "x") [("y",TrueVal),("w",FalseVal),("s",IntVal 3),("x",ListVal [StringVal "text"])] @?= (Right (ListVal [StringVal "text"]),[]),
  testCase "check x EBadVar env list of not x vars" $
    runComp (look "x") [("y",TrueVal),("w",FalseVal),("s",IntVal 3),("x1",ListVal [StringVal "text"])] @?= (Left (EBadVar "x"),[])
   ],
   testGroup "test2"
   [testCase "withBinding NoneVal x" $
    runComp (withBinding "x" NoneVal (look "x")) [] @?= (Right NoneVal,[]),
  testCase "withBinding IntVal x" $
    runComp (withBinding "x" (IntVal 3) (look "x")) [] @?= (Right (IntVal 3),[]),
  testCase "withBinding TrueVal x" $
    runComp (withBinding "x" (TrueVal) (look "x")) [] @?= (Right TrueVal,[]),
  testCase "withBinding FalseVal x" $
    runComp (withBinding "x" (FalseVal) (look "x")) [] @?= (Right FalseVal,[]),
  testCase "withBinding StringVal x" $
    runComp (withBinding "x" (StringVal "text") (look "x")) [] @?= (Right (StringVal "text"),[]),
  testCase "withBinding ListVal StringVal x" $
    runComp (withBinding "x" (ListVal [StringVal "text"]) (look "x")) [] @?= (Right (ListVal [StringVal "text"]),[]),
  testCase "withBinding ListVal Vals x" $
    runComp (withBinding "x" (ListVal [StringVal "text",TrueVal, FalseVal]) (look "x")) [] @?= (Right (ListVal [StringVal "text",TrueVal, FalseVal]),[])],
   testGroup "output"
   [
      testCase "output empty string" $
      runComp (output "") [] @?= (Right (),[""]),
      testCase "output text" $
      runComp (output "asdasdasdasd") [] @?= (Right (),["asdasdasdasd"])
    ],
   testGroup "Truthy"
   [testCase "truthy FalseVal" $
    truthy FalseVal @?= False,
  testCase "truthy (IntVal 10)" $
    truthy (IntVal 10) @?= True,
  testCase "truthy TrueVal" $
    truthy TrueVal @?= True,
  testCase "truthy (IntVal 3)" $
    truthy (IntVal 3) @?= True,
  testCase "truthy (ListVal [NoneVal])" $
    truthy (ListVal [NoneVal]) @?= True],
   testGroup "operate"
   [testCase "operate Plus" $
    operate Plus (IntVal 0) (IntVal 1) @?= Right (IntVal 1),
  testCase "operate Mod by Zero" $
    operate Mod (IntVal 0) (IntVal 0) @?= Left "Can't Mod by 0",
  testCase "test operate Eq" $
    operate Eq (IntVal 1) (IntVal 1) @?= Right TrueVal,
  testCase "operate Less 0<0" $
    operate Less (IntVal 0) (IntVal 0) @?= Right FalseVal,
  testCase "operate Greater 0>0" $
    operate Greater (IntVal 0) (IntVal 0) @?= Right FalseVal,
  testCase "operate In by []" $
    operate In (IntVal 0) (ListVal []) @?= Right FalseVal,
  testCase "operate Plus by IntVal" $
    operate Plus (IntVal 30) (IntVal 10) @?= Right (IntVal 40),
  testCase "operate Minus Negative" $
    operate Minus (IntVal 0) (IntVal 3) @?= Right (IntVal (-3)),
  testCase "operate Times by 10" $
    operate Times (IntVal 1) (IntVal 10) @?= Right (IntVal 10),
  testCase "operate Div by 4" $
    operate Div (IntVal 0) (IntVal 4) @?= Right (IntVal 0),
  testCase "operate Eq by Different Values" $
    operate Eq (IntVal 0) (TrueVal) @?= Right FalseVal,
  testCase "operate Eq StringVal" $
    operate Eq (StringVal "test") (StringVal "test") @?= Right TrueVal,
  testCase "operate Eq []@?=4" $
    operate Eq (ListVal [IntVal 0]) (IntVal 4) @?= Right FalseVal,
  testCase "operate Less 4<0" $
    operate Less (IntVal 4) (IntVal 0) @?= Right FalseVal,
  testCase "operate Greater 4>0" $
    operate Greater (IntVal 4) (IntVal 0) @?= Right TrueVal,
  testGroup "eval"
    [testCase "eval (1+5+8)" $
    runComp (eval (Oper Plus (Oper Plus (Const (IntVal 1)) (Const (IntVal 5))) (Const (IntVal 8)))) [] @?= (Right (IntVal 14),[]),
  testCase "eval not x (x not in env)" $
    runComp (eval (Not (Var "x"))) [] @?= (Left (EBadVar "x"),[]),
  testCase "eval not 1" $
    runComp (eval (Not (Const (IntVal 1)))) [] @?= (Right FalseVal,[]),
  testCase "eval not 23" $
    runComp (eval (Not (Const (IntVal 23)))) [] @?= (Right FalseVal,[]),
  testCase "eval not empty" $
    runComp (eval (Not (Const (StringVal "")))) [] @?= (Right TrueVal,[]),
  testCase "eval not text" $
    runComp (eval (Not (Const (StringVal "text")))) [] @?= (Right FalseVal,[]),
  testCase "eval not NoneVal" $
    runComp (eval (Not (Const NoneVal))) [] @?= (Right TrueVal,[]),
  testCase "eval Not x Trueval in env" $
    runComp (eval (Not (Var "x"))) [("x",TrueVal)] @?= (Right FalseVal,[]) ,
  testCase "eval Not x Trueval in env" $
    runComp (eval (Not (List []))) [] @?= (Right TrueVal,[]),
  testCase "eval Call range(1,x) x unbound" $
    runComp (eval (Call "range" [Const (IntVal 1), Var "x"])) [] @?= (Left (EBadVar "x"),[]),
  testCase "eval Call range(1,x) x bound 4" $
    runComp (eval (Call "range" [Const (IntVal 1), Var "x"])) [("x",IntVal 4)] @?= (Right (ListVal [IntVal 1,IntVal 2,IntVal 3]),[]),
  testCase "eval Call range(5)" $
    runComp (eval (Call "range" [Const (IntVal 4)])) [] @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3]),[]),
  testCase "eval Call range(1,8,2)" $
    runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 8), Const (IntVal 2)])) [] @?= (Right (ListVal [IntVal 1,IntVal 3,IntVal 5,IntVal 7]),[]),
  testCase "eval Call range(1,8)" $
    runComp (eval (Call "range" [Const (IntVal 1), Const (IntVal 8)])) [] @?= (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7]),[]),
  testCase "eval List (NoneVal, 2)" $
    runComp (eval (List [Const NoneVal, Const (IntVal 2)])) [] @?=(Right (ListVal [NoneVal,IntVal 2]),[]),
  testCase "eval List (NoneVal)" $
    runComp (eval (List [Const NoneVal])) [] @?= (Right (ListVal [NoneVal]),[]),
  testCase "eval List []" $
    runComp (eval (List [])) [] @?= (Right (ListVal []),[]),
  testCase "eval List (4,1,NoneVal, string)" $
    runComp (eval (List [Const (IntVal 4),Const (IntVal 1), Const NoneVal,Const (StringVal "1")])) [] @?= (Right (ListVal [IntVal 4,IntVal 1,NoneVal,StringVal "1"]),[]),
  testCase "eval Compr x" $
    runComp (eval (Compr (Var "x") [])) [("x",IntVal 4)] @?= (Right (ListVal [IntVal 4]),[]),
  testCase "eval Compr EBadVar" $
    runComp (eval (Compr (Var "x") [])) [] @?= (Left (EBadVar "x"),[]),
  testCase "eval Compr (x+2) for range (1,5)" $
    runComp (eval (Compr (Oper Plus (Var "x") (Const (IntVal 2))) [CCFor "x" (Call "range" [Const (IntVal 1),Const (IntVal 5)])])) [("x",IntVal 4)] @?= (Right (ListVal [IntVal 3,IntVal 4,IntVal 5,IntVal 6]),[])],
   testGroup "exec Tests"
     [testCase "exec []" $
    runComp (exec []) [] @?= (Right (),[]),
  testCase "exec [print(x)]" $
    runComp (exec [SDef "x" (Call "print" [(Var "x")])]) [("x",IntVal 4)] @?= (Right (),["4"])]
  ]
  ]
