data Expr = Val Int | Div Expr Expr
a = Val 1
b = Val 2
c = Val 0
d = Div a b
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = eval x Prelude.>>= \n ->
                 eval y Prelude.>>= \m ->
                 safediv n m

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = do x1 <- eval2 x
                     x2 <- eval2 y
                     safediv x1 x2
-- formal definition of monads
-- class Applicative m => Monad m where
--     (>>=) :: m a -> (a -> m b) -> m b
--     return :: a -> m a
-- instance Monad Maybe where
--     (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--     Nothing >>= f = Nothing
--     (Just x) >>= f = f x

-- instance Monad [] where
--     (>>=) :: [a] -> (a -> [b]) -> [b]
--     xs >>= f = concat (map f xs)

-- Monadic programming uses return
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

pairs2 :: [a] -> [b] -> [(a,b)]
pairs2 xs ys = [(x,y)|x<-xs,y<-ys]

type State = Int

newtype ST a = S(State -> (a,State))

app::ST a -> State ->(a,State)
app (S st) s = st s

instance Monad Main.ST where
    return x = S(\s -> (x,s))
    st >>= f = S(\s -> let (x,s') = app st s
                       in app (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

t :: Tree Integer
t = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

t2 :: Tree Char
t2 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- relabelling a tree
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where (l',n') = rlabel l n
                            (r',n'') = rlabel r n'

fresh :: ST Int
fresh = S(\n -> (n,n+1))

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do Leaf <$> fresh
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

label :: Tree a -> Tree Int
label t = fst (app (mlabel t) 0)