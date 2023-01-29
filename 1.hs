convert :: [char] -> [char]
convert [] = []
convert (x:xs) = case xs of
    ["C"] -> 5/9*(x-32)
    ["F"] -> 9/5*x+32
    _ -> error "Invalid input"

