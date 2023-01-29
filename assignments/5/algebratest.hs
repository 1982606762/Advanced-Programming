import Test.QuickCheck

data Expr = Cst Int
    | Add Expr Expr
    deriving (Eq, Show)

exprN 0 = fmap Cst arbitrary
exprN n = oneof [
    fmap Cst arbitrary, 
    do {x <- exprN (n-1); 
    y <- exprN (n-1); 
    return (Add x y)}]

instance Arbitrary Expr where
    arbitrary = sized exprN

eval :: Expr -> Int
eval (Add x y) = eval x + eval y
eval (Cst x) = x

prop_CommutativeAddition :: Expr -> Expr -> Property
prop_CommutativeAddition x y = eval (Add x y) === eval (Add y x)
