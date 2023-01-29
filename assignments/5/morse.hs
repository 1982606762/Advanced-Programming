module Morse (encode,decode) where
import Data.List (stripPrefix)
table :: [(Char, String)]

table = [('A', ".-"), ('N', "-."), ('B', "-..."), ('O', "---"), ('C', "-.-."), ('P', ".--."), ('D', "-.."), ('Q', "--.-"), ('E', "."), ('R', ".-."), ('F', "..-."), ('S', "..."), ('G', "--."), ('T', "-"), ('H', "...."), ('U', "..-"), ('I', ".."), ('V', "...-"), ('J', ".---"), ('W', ".--"), ('K', "-.-"), ('X', "-..-"), ('L', ".-.."), ('Y', "-.--"), ('M', "--"), ('Z', "--..")]

encode :: String -> String
encode = foldr (\ x -> (++) (fromJust (lookup x table))) []

fromJust :: Maybe String -> [Char]
fromJust = concat

decode :: String -> [String]
decode "" = [""]
decode m = concatMap
    (\(letter, code) ->
        case stripPrefix code m of
            Just rest -> map (letter:) (decode rest)
            Nothing -> []
    )
    table