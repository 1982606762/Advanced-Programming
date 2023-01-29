import Text.Read (Lexeme(String))
type VName = String
type Env = VName -> Maybe Integer
data Exp = 
    Cst Integer 
    | Add Exp Exp 
    | Sub Exp Exp 
    | Mul Exp Exp 
    | Div Exp Exp 
    | Pow Exp Exp 
    | If {test, yes, no :: Exp} 
    | Var VName 
    | Let {var :: VName, def, body :: Exp} 
    | Sum {var :: VName, from, to, body :: Exp}
a = Cst 2
b = Cst 3
c = Pow (Div(Mul (Sub (Add a b) a) a) a) a
d = Cst (-1)
e = Pow a d
f = Let "a" (Cst 42) (Var "a")
initEnv = \v -> Nothing
-- 1.1
showExp :: Exp -> String
showExp (Cst x) = "Cst " ++ show x
showExp (Add x y) = "(" ++ showExp x ++ " + " ++ showExp y ++ ")"
showExp (Sub x y) = "(" ++ showExp x ++ " - " ++ showExp y ++ ")"
showExp (Mul x y) = "(" ++ showExp x ++ " * " ++ showExp y ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ " `div` " ++ showExp y ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ " ^ " ++ showExp y ++ ")"
showExp a = error "undefined exp!!!"
-- 1.2
evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) = evalSimple x `div` evalSimple y
evalSimple (Pow x y)
    | evalSimple y < 0 = error "Pow negative"
    | otherwise = evalSimple x ^ evalSimple y
evalSimple a = error "undefined exp!!!"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n e = \v' -> if v == v' then Just n else e v'


evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull (Add x y) e = evalFull x e + evalFull y e
evalFull (Sub x y) e = evalFull x e - evalFull y e
evalFull (Mul x y) e = evalFull x e * evalFull y e
evalFull (Div x y) e = evalFull x e `div` evalFull y e
evalFull (Pow x y) e
    | evalFull y e < 0 = error "Pow negative"
    | otherwise = evalFull x e ^ evalFull y e
evalFull (If test yes no) e
    | evalFull test e == 0 = evalFull no e
    | otherwise = evalFull yes e
evalFull (Var x) e = case e x of
    Just x -> x
    Nothing -> error "undefined var!!!"
evalFull (Let var def body) e = 
    let ne = extendEnv var evaldef e
        evaldef = evalFull def e
    in evalFull body ne
evalFull (Sum var from to body) e = 
    let ne = extendEnv var evalfrom e
        evalfrom = evalFull from e
        evalto = evalFull to e
    in sum [evalFull body (extendEnv var i ne) | i <- [evalfrom..evalto]]

data ArithError = EBadVar VName 
    | EDivZero 
    | ENegPower 
    | EOther String
    deriving (Eq, Show)

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst e) _ = Right e
evalErr (Add x y) e = case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> Right (x + y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Sub x y) e = case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> Right (x - y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Mul x y) e = case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> Right (x * y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Div x y) e = case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> if y == 0 
        then Left EDivZero 
        else Right (x `div` y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (Pow x y) e = case (evalErr x e, evalErr y e) of
    (Right x, Right y) -> if y < 0 
        then Left ENegPower 
        else Right (x ^ y)
    (Left x, _) -> Left x
    (_, Left y) -> Left y
evalErr (If test yes no) e = case evalErr test e of
    Right x -> if x == 0 
        then evalErr no e 
        else evalErr yes e
    Left x -> Left x
evalErr (Var x) e = case e x of
    Just x -> Right x
    Nothing -> Left (EBadVar x)
evalErr (Let var def body) e = case evalErr def e of
    Right x -> evalErr body (extendEnv var x e)
    Left x -> Left x
evalErr (Sum var from to body) e = case (evalErr from e, evalErr to e) of
    (Right x, Right y) -> Right (sum [evalFull body (extendEnv var x e) | x <- [x..y]])
    (Left x, _) -> Left x
    (_, Left y) -> Left y
