import Test.QuickCheck
import Morse

data UpperCaseString = UpperCaseString String deriving (Eq, Show)
a = foldl (\x y -> x ++ [y]) "" ['A'..'Z']
instance Arbitrary UpperCaseString where
    arbitrary = do
        n <- chooseInt (0, 5)
        str <- vectorOf n (elements ['A'..'Z'])
        return (UpperCaseString str)


prop_EncodeDecode :: UpperCaseString -> Bool
prop_EncodeDecode (UpperCaseString str) = str `elem` decode (encode str)



