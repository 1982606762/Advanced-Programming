import Test.QuickCheck 


prop_CommutativeAddition :: Int -> Int -> Property 
prop_CommutativeAddition x y = x + y === y + x 
prop_Distributive :: Int -> Int -> Int -> Property
prop_Distributive x y z = (x + y) * z === x * z + y * z