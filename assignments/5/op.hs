import Test.QuickCheck
import Morse
import Prelude
import Data.List
data UppercaseString = UppercaseString String deriving (Eq, Show)



instance Arbitrary UppercaseString where
    arbitrary = do
        n <- chooseInt (0, 5)
        str <- vectorOf n (elements ['A'..'Z'])
        return (UppercaseString str)

selfsum :: [Int] -> Int
selfsum ints = sum ints

selfmerge :: Ord a => [a] -> [a] -> [a]
selfmerge a b = sort (a ++ b)

prop_selfsum :: [Int] -> Property
prop_selfsum xs = sum xs === selfsum xs

prop_sum2 :: [Int] -> [Int] -> Property
prop_sum2 xs ys = (sum xs + sum ys) === sum (xs ++ ys)

prop_selfmerge :: [Int] -> [Int] -> Property
prop_selfmerge xs ys = selfmerge xs ys ===  sort(xs ++ ys)

prop_morse :: UppercaseString -> Bool
prop_morse (UppercaseString str) = str `elem` decode (encode str)


