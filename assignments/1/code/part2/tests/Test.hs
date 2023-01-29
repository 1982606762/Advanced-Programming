-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [
  ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4),
  ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42),
  ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x")),


  ("testSimple-test1", evalSimple (Add (Cst 2) (Sub (Cst 20)(Cst 4))) == 18),
  ("testSimple-test2", evalSimple ( Mul (Cst 20) (Add (Cst 2) (Cst 2))) == 80),
  ("testSimple-test3", evalSimple  (Div (Pow (Cst 2)(Cst 0)) (Add (Cst 2) (Cst 2))) == 0),
  ("testSimple-test4", evalSimple (Pow (Div (Pow (Cst 2)(Cst 0)) (Add (Cst 2) (Cst 2))) (Cst 0)) == 1),

  ("evalErr-test0", evalErr (Add (Div (Cst 20)(Cst 0)) (Cst 4)) initEnv == Left EDivZero),
  ("evalErr-test1", evalErr (Sub (Cst 20)(Cst 4)) initEnv == Right 16),
  ("evalErr-test2", evalErr (Add (Cst 20)(Cst 4)) initEnv == Right 24),
  ("evalErr-test3", evalErr (Mul (Cst 20)(Cst 4)) initEnv == Right 80),
  ("evalErr-test4", evalErr (Pow (Cst 2)(Cst 4)) initEnv == Right 16),
  ("evalErr-test5", evalErr (Pow (Cst 2)(Cst 0)) initEnv == Right 1),
  ("evalErr-test6", evalErr (Pow (Cst 2)(Cst (-1))) initEnv == Left ENegPower),
  ("evalErr-test7", evalErr (Div (Cst 20)(Cst 4)) initEnv == Right 5),
  ("evalErr-test8", evalErr (Div (Cst 20)(Cst 0)) initEnv == Left EDivZero),
  ("evalErr-test9", evalErr (Add (Cst 20) (Div (Cst 20)(Cst 0))) initEnv == Left EDivZero),
  ("evalErr-test10", evalErr (Div (Let {var = "x", def = Cst 10, body = Div (Var "x") (Cst 9) }) (Add (Mul (Cst 3) (Cst 3)) (Cst (-8)))) initEnv == Right 1),
  ("evalErr-test11", evalErr (Div (Let {var = "x", def = Cst 10, body = Div (Var "x") (Cst 0) }) (Add (Mul (Cst 3) (Cst 3)) (Cst (-8)))) initEnv == Left EDivZero),
  ("evalErr-test12", evalErr (Div (Let {var = "x", def = Cst 10, body = Div (Var "x") (Cst 9) }) (Add (Pow (Cst 3) (Cst (-1))) (Cst (-8)))) initEnv == Left ENegPower),
  ("evalErr-test13", evalErr (Div (Let {var = "x", def = Cst 10, body = Div (Var "x") (Let {var = "x", def = Add (Var "x") (Cst 2), body = Sub (Var "x") (Cst 3)}) }) (Add (Mul (Cst 3) (Cst 3)) (Cst (-8)))) initEnv == Right 1)]
main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
