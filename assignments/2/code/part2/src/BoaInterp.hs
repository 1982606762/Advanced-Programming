-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp(const (Right a, []))
  m >>= f = Comp(\e -> case runComp m e of
    (Left a,list) -> (Left a,list)
    (Right a,list) -> case runComp (f a) e of
      (Left a,s) -> (Left a,list <> s)
      (Right a,s) -> (Right a,list <> s)
    )

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort (EBadVar a) = Comp(const (Left (EBadVar a), []))
abort (EBadFun a) = Comp(const (Left (EBadFun a), []))
abort (EBadArg a) = Comp(const (Left (EBadArg a), []))

look :: VName -> Comp Value
look a = Comp(\e -> case lookup a e of
  Just x -> (Right x,[])
  Nothing -> (Left (EBadVar a),[])
  )

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp(\e -> let ne = (x,v):e
                              in runComp m ne)

output :: String -> Comp ()
output s = Comp(const (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy FalseVal = False
truthy (IntVal 0) = False
truthy (StringVal "") = False
truthy (ListVal []) = False
truthy _ = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal(v1+v2))
operate Plus _ _ = Left "can only plus integer velue"
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))
operate Minus _ _ = Left "can only minus integer value"
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))
operate Times _ _ = Left "can only time integer value"
operate Div (IntVal v1) (IntVal v2)=
  if v2 == 0
    then Left "Divide by zero"
    else Right (IntVal (v1 `div` v2))
operate Div _ _ = Left "can only div integer value"
operate Mod (IntVal v1) (IntVal v2)=
  if v2 == 0
    then Left "Can't Mod by 0"
    else Right (IntVal (v1 `mod` v2))
operate Mod _ _ = Left "can only mod integer value"
operate Eq v1 v2 =
  if v1 == v2
    then Right TrueVal
    else Right FalseVal
operate Less (IntVal v1) (IntVal v2)=
  if v1 < v2
    then Right TrueVal
    else Right FalseVal
operate Less _ _ = Left "can only compare integer value"
operate Greater (IntVal v1) (IntVal v2) =
  if v1 > v2
    then Right TrueVal
    else Right FalseVal
operate Greater _ _ = Left "can only compare integer value"
operate In v1 (ListVal v2) =
  if v1 `elem` v2
    then Right TrueVal
    else Right FalseVal
operate _ _ _ = Left "undefined Operation"

range :: Value -> Value -> Value -> [Value]
range (IntVal v1) (IntVal v2) (IntVal v3)
  | v3 > 0 && v1 < v2 = IntVal v1 : range (IntVal (v1 + v3)) (IntVal v2) (IntVal v3)
  | v3 < 0 && v1 > v2 = IntVal v1 : range (IntVal (v1 + v3)) (IntVal v2) (IntVal v3)
  | otherwise = []
range _ _ _ = []


-- get the string of a value
getexpstring :: Value -> String
getexpstring NoneVal = "None"
getexpstring TrueVal = "True"
getexpstring FalseVal = "False"
getexpstring (IntVal x) = show x
getexpstring (StringVal s) = s
getexpstring (ListVal x) = "[" ++ stringfylist x ++ "]"

-- help to make list become a string
stringfylist :: [Value] -> String
stringfylist [] = ""
stringfylist [x] = getexpstring x
stringfylist (x:xs) = getexpstring x ++ ", " ++ stringfylist xs

-- print the value list
printFunc :: [Value] -> String
printFunc [] = ""
printFunc [x] = getexpstring x
printFunc (x:xs) = getexpstring x ++ " " ++ printFunc xs

apply :: FName -> [Value] -> Comp Value
apply "range" [v1,v2,NoneVal] = abort (EBadArg "bad arg")
apply "range" [v1,v2,v3] = let tmp = range v1 v2 v3
  in return (ListVal tmp)
apply "range" [v1] = apply "range" [IntVal 0,v1,IntVal 1]
apply "range" [v1,v2] = apply "range" [v1,v2,IntVal 1]
apply "range" _ = abort (EBadArg "range can only deal <= 3 paras")
apply "print" s = do
  output (printFunc s)
  return NoneVal
apply a _ = abort (EBadFun a)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var v) = look v
eval (Oper op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case operate op v1 v2 of
    Right x -> return x
    Left x -> abort (EBadArg x)
eval (Not v) = do
  tmp <- eval v
  if truthy tmp then return FalseVal else return TrueVal
eval (Call f es) = do
  vs <- eval (List es)
  case vs of
    (ListVal x) -> apply f x
    _ -> abort (EBadArg "call function err")
eval (List []) = return (ListVal [])
eval (List (e:es)) = do
  x <- eval e
  xs <- eval (List es)
  case xs of
    ListVal a -> return (ListVal (x : a))
    _ -> abort(EBadArg "List error")
eval (Compr e []) = do
  x <- eval e
  return (ListVal [x])
eval (Compr e ((CCFor v q):qs)) = do
    v1 <- eval q
    case v1 of
      ListVal l -> do
        -- l' <- mapM (\x -> withBinding v x (eval (Compr e qs))) l
        -- return (ListVal l')
        l' <- mapM (\x -> withBinding v x (eval e)) l
        return (ListVal l')
      _ -> abort (EBadArg "CCFor error,not a list")
eval (Compr e ((CCIf a):_)) = do
    check <- eval a
    let b = truthy check
    if b
      then do res<-eval e
              return (ListVal [res])
    else return (ListVal [])

exec :: Program -> Comp ()
exec [] = return ()
exec ((SDef v e):ps) = do
  x <- eval e
  withBinding v x (exec ps)
exec ((SExp e):ps) = do
  eval e
  exec ps

execute :: Program -> ([String], Maybe RunError)
execute p =  case runComp (exec p) [] of
  (Right (), s) -> (s, Nothing)
  (Left err, s) -> (s, Just err)
