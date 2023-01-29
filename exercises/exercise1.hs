import qualified Data.Map as Map
import Data.Char
import qualified Data.List as DList
type Pos = (Int, Int)
data Direction = North | South | East | West
-- 1.
move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
-- 2.
moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x, y)
moves (d:ds) (x,y) = moves ds (move d (x,y) )
-- 3.
data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord)

addingNumber :: Nat -> Nat -> Nat
addingNumber Zero y = y
addingNumber x Zero = x
addingNumber x (Succ y) = addingNumber (Succ x) y

multiplyNat :: Nat -> Nat -> Nat
multiplyNat Zero y = Zero
multiplyNat x Zero = Zero
multiplyNat (Succ Zero) y = y
multiplyNat x (Succ Zero) = x
multiplyNat x (Succ y) = addingNumber (multiplyNat x y) x
-- 4.
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = nat2int x+1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat x)
-- 5.
data Tree = Leaf | Node Int Tree Tree
    deriving (Eq, Show, Read, Ord)

-- Why does compiler say there's some patterns not matched
insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y1 y2 y3)
    | x > y1 = Node y1 y2 (insert x y3)
    | x < y1 = Node y1 (insert x y2) y3
    | x == y1 = Node x y2 y3
-- 6.
-- What's a polymorphic tree...?
data PTree = PLeaf | PNode Int PTree PTree
    deriving (Eq, Show, Read, Ord)

-- 7.
m :: Map.Map String String
m = Map.fromList [("A",".-"),("B","-..."),("C","-.-."),("D","-.."),("E","."),("F","..-."),("G","--."),("H","...."),("I",".."),("J",".---"),("K","-.-"),("L",".-.."),("M","--"),("N","-."),("O","---"),("P",".--."),("Q","--.-"),("R",".-."),("S","..."),("T","-"),("U","..-"),("V","...-"),("W",".--"),("X","-..-"),("Y","-.--"),("Z","--..")]
stringList :: [String]
stringList = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"]
morseList :: [String]
morseList = [".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--.."]
find :: String -> String
find x = Map.findWithDefault "" x m

encode :: String -> String
encode = foldr (\ x -> (++) (Main.find [toUpper x])) []

-- 8.
-- doesn't have a clue how to do it...
-- 9.
class Sizeable t where
  size :: t -> Int

instance Sizeable Int where
  size _ = 1

instance Sizeable Char where
  size _ = 1

instance Sizeable [a] where 
    size [] = 0
    size (x:xs) = 1 + size xs

-- instance Sizeable a => Sizeable [a] where
--     size[] = 0
--     size(x:xs) = size x + size xs

instance Sizeable Tree where
    size Leaf = 0
    size (Node y1 y2 y3)  = 1 + size y2 + size y3




-- test
main :: IO ()
main = print (size ["qwe","q"])
