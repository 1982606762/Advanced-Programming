-- Put yor transformer implementation in this file
module TransformerImpl where

import Definitions

convert :: EGrammar -> EM Grammar
convert [] = Right []
convert [(lhs, rhs)] = case rhs of
    EBar r1 r2 -> case (r1, r2) of 
        (ESeq s1 u1, ESeq s2 u2) -> Right [(lhs, [(removeEsimple s1, AUser u1), (removeEsimple s2, AUser u2)])];
        _ -> Left "Error"
    ESeq s u -> Right [(lhs, [(removeEsimple s, AUser u)])]      
    ESimple s -> Right [(lhs, [([s], AUser "")])]
    _ -> Left "Error"
convert ((lhs, rhs):xs) = 
    case convert [(lhs, rhs)] of
        Right r -> case convert xs of
            Right rs -> Right (r ++ rs)
            Left e -> Left e
        Left e -> Left e    
    

removeEsimple :: [ERHS] -> [Simple]
removeEsimple [] = []
removeEsimple (x:xs) = case x of
    ESimple s -> s : removeEsimple xs

    
lre :: Grammar -> EM Grammar
lre = undefined

lfactor :: Grammar -> EM Grammar
lfactor = undefined
