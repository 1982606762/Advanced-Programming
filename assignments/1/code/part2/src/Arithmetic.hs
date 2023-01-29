-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst x)     
    | x >= 0 = show x
    | otherwise = "(" ++ show x ++ ")"
showExp (Add x y) = "(" ++ showExp x ++ " + " ++ showExp y ++ ")"
showExp (Sub x y) = "(" ++ showExp x ++ " - " ++ showExp y ++ ")"
showExp (Mul x y) = "(" ++ showExp x ++ " * " ++ showExp y ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ " `div` " ++ showExp y ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ " ^ " ++ showExp y ++ ")"
-- There's a warning here,I use this line to report problems what 1.1 last said
showExp a = error "undefined exp!!!"

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) 
    | evalSimple y == 0 = error "Div zero"
    | otherwise = evalSimple x `div` evalSimple y
evalSimple (Pow x y)
    | evalSimple y < 0 = error "Pow negative"
    | evalSimple x == 0 = if evalSimple y == 0 then 1 else 0
    | otherwise = evalSimple x ^ evalSimple y
-- There's a warning here,I use this line to report problems what 1.2 last said
evalSimple a = error "undefined exp!!!"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n env = \v' -> if v == v' then Just n else env v'

evalFull :: Exp -> Env -> Integer
evalFull (Cst e) _ = e
evalFull (Add x y) e = evalFull x e + evalFull y e
evalFull (Sub x y) e = evalFull x e - evalFull y e
evalFull (Mul x y) e = evalFull x e * evalFull y e
evalFull (Div x y) e = evalFull x e `div` evalFull y e
evalFull (Pow x y) e
    | evalFull y e < 0 = error "Pow negative"
    | evalFull x e == 0 = if evalFull y e == 0 then 1 else 0
    | otherwise = evalFull x e ^ evalFull y e
evalFull (If test yes no) e
    | evalFull test e == 0 = evalFull no e
    | otherwise = evalFull yes e
evalFull (Var x) e = case e x of
    Just x -> x
    Nothing -> error "undefined var!!!"
evalFull (Let var def body) e = 
    let evaldef = evalFull def e
        ne = extendEnv var evaldef e
    in evalFull body ne
evalFull (Sum var from to body) e = 
    let evalfrom = evalFull from e
        evalto = evalFull to e
        ne = extendEnv var evalfrom e
    in sum [let nne = extendEnv var x ne in evalFull body nne| x <- [evalfrom..evalto]]

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst e) _ = Right e
evalErr (Add x y) e =
    case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> Right (x + y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Sub x y) e = 
    case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> Right (x - y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Mul x y) e = 
    case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> Right (x * y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Div x y) e = 
    case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> 
        if y == 0 
        then Left EDivZero 
        else Right (x `div` y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Pow x y) e = 
    case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> 
        if y < 0 
        then Left ENegPower 
        else Right (x ^ y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (If test yes no) e = 
    case evalErr test e of
    Right x | x == 0 -> evalErr no e
            | otherwise -> evalErr yes e
    Left x -> Left x
evalErr (Var x) e = 
    case e x of
    Just x -> Right x
    Nothing -> Left (EBadVar x)
evalErr (Let var def body) e = 
    case evalErr def e of
    Right x -> 
        let evaldef = evalErr def e
            ne = extendEnv var x e
        in evalErr body ne
    Left x -> Left x
evalErr (Sum var from to body) e = 
    case (evalErr from e, evalErr to e) of
    (Right x, Right y) -> Right (
        sum [let nne = extendEnv var x e in evalFull body nne| x <- [x..y]])
    (Left x, _) -> Left x
    (_, Left y) -> Left y

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
