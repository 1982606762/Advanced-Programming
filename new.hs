import Distribution.Simple.Utils (xargs)
import Data.Char
import Data.List
import System.Directory(getCurrentDirectory, doesFileExist)
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky 8 = "LUCKY NUMBER eight!"  
lucky x = "Sorry, you're out of luck, pal!"
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)
head' :: (Show a) => [a] -> String
head' [] ="Can't call head on an empty list, dummy!"  
head' (x:[]) = "x"
head' (x:y:[]) = show x ++ show y
head' (x:y:z:_) = "x" ++ "y" ++ "z"

newtest :: [a] -> a
newtest [] = error "empty"
newtest (x:_) = x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (s:se) = reverse' se ++ [s]

plusone :: (Integral a) => a -> a
plusone = subtract 1

multwith6 :: (Num a) => a -> a -> a
multwith6 x y = x*y*6

times9 :: (Num a )=>a->a
times9 x = x*9

applytriple :: (a -> a -> a) -> a -> a -> a
applytriple f x y = f (f (f x y) y) y

applytwice :: (a -> a -> a) -> a -> a -> a
applytwice f x y = f(f x y) y

apply2 :: (a->a)->a->a
apply2 f x = f (f x)

apply3 :: (Num a) => (a->a)->a->a
apply3 f x = f (f (f x))

findbig :: (Integral a) => a
findbig = head (filter p [100000,99999..])
    where p x = x `plus` 3829 == 9283

plus :: (Integral a) => a -> a -> a
plus x y = x + y

converse :: String -> String
converse [] = []
converse (x:xs)
    | x == ' ' = converse xs
    | x == 'a' = 'b':converse xs
    | otherwise = x:converse xs

type NewWord = String

usewords :: NewWord -> String
usewords [] = []
usewords (x:xs) 
    |x == ' ' =  '-': usewords xs
    |otherwise = x:usewords xs

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
a = Circle  1 2 3

solveRPN ::(Num a, Read a) => String -> a
solveRPN = head . foldl foldingfuc [] . words  
    where foldingfuc (x:y:ys) "*" = (x*y) : ys
          foldingfuc (x:y:ys) "+" = (x+y) : ys
          foldingfuc (x:y:ys) "-" = (y - x) : ys
          foldingfuc xs str = read str : xs



foo :: [Int] -> [Int] -> Int
foo [] [] = error "empty!"
foo _ [_] = 1
foo [x] [y,z] | x > y = x+z
foo x (y:_) = y
foo (x:_) _ = x

wordcount :: String -> Int
wordcount x =length (words x) 

double ::(Integral a) =>a->a
double x = x + x

-- main = do
    -- putStrLn (solveRPN "10 4 3 + 2 * -")