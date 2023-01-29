-- Put yor parser implementation in this file
module ParserImpl where

import Definitions
-- import ReadP or Parsec
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char

type Parser a = ReadP a

type ParseError = String


-- Spec ::= preamble ERules.
parseSpec :: String -> EM (String, EGrammar)
parseSpec str = case readP_to_S (do
  preamble <- parsePreamble
  eRules <- parseErules
  eof
  return (preamble, eRules)) str of
  [] -> Left "Parse error"
  x -> case last x of
    (x, "") -> return x
    _ -> Left "Parse error"


spec :: Parser (String, EGrammar)
spec = do
    preamble <- parsePreamble
    eRules <- parseErules
    return (preamble, eRules)

-- ERules ::= ERule | ERule ERules.
parseErules :: Parser [ERule]
parseErules = do
            skipmore        
            e <- parseErule
            es <- parseErules
            return (e:es)
        <|> do
            e <- parseErule
            return [e]

-- ERule :: LHS "::=" Alts ".".
parseErule :: Parser ERule
parseErule = do
            (l1,l2,l3) <- parseLhs
            symbol "::="
            a <- parseAlts
            schar '.'
            return ((l1,l2,l3),a)

-- LHS ::= name OptType | "_".
parseLhs :: Parser RLHS
parseLhs = do
        skipmore
        n <- parseName
        o <- parseOptType
        if isUpper (head n)
            then return (n, RPlain, o)
            else return (n, RToken, o)
    <|> do
        skipmore
        symbol "_"
        return ("_",RSep,Nothing)

-- OptType ::= | "{:" htext "}".
parseOptType :: Parser (Maybe Type)
parseOptType = do
            schar '{'
            schar ':'
            h <- parseHtext
            schar '}'
            return (Just (AUser h))
        <|> return Nothing


-- Alts ::= Seq | Seq "|" Alts.
parseAlts :: Parser ERHS
parseAlts = do
        skipmore
        s <- parseSeq
        symbol "|"
        EBar s <$> parseAlts
       <|> do
        skipmore
        parseSeq

-- Seq ::= Simple | Simplez "{" htext "}".
parseSeq :: Parser ERHS
parseSeq = do
        s <- parseSimplez
        schar '{'
        h <- parseHtext
        schar '}'
        return (ESeq s h)
    <|> parseSimple

-- Simplez ::= | Simple Simplez.
parseSimplez :: Parser [ERHS]
parseSimplez = do
            s <- parseSimple
            ss <- parseSimplez
            return (s:ss)
        <|> return []

-- Simple ::= Simple0 | Simple0 "?" | Simple0 "*" | "!" Simple0.
parseSimple :: Parser ERHS
parseSimple = do
            s <- parseSimple0
            schar '?'
            return (EOption s)
        <|> do
            s <- parseSimple0
            schar '*'
            return (EMany s)
        <|> do
            schar '!'
            ENot <$> parseSimple0
        <|> parseSimple0

-- Simple0 ::= Atom Simple0'.
parseSimple0 :: Parser ERHS
parseSimple0 = do
            a <- parseAtom
            parseSimple0' a

-- Simple0' ::= | "{?" htext "}" Simple0'.
parseSimple0' :: ERHS -> Parser ERHS
parseSimple0' a = do
                schar '{'
                schar '?'
                h <- parseHtext
                schar '}'
                parseSimple0' (EPred a h)
            <|> return a


-- Atom ::= name | tokLit | "@" | charLit | "(" Alts ")".
parseAtom :: Parser ERHS
parseAtom = do
        ESimple . SNTerm <$> parseName
    <|> do
        ESimple . SLit <$> parseTokLit
    <|> parseAt
    <|> do
        ESimple . SChar <$> parseCharLit
    <|> do
        schar '('
        a <- parseAlts
        schar ')'
        return a

parseName :: Parser String
parseName = token(do
        c <- satisfy isLetter
        cs <- munch (\c -> isLetter c || isDigit c || c == '_')
        return (c:cs))

parseTokLit :: Parser String
parseTokLit = token(do
        schar '"'
        cs <- munch (\c -> c /= '"')
        schar '"'
        return cs)

    

parseCharLit :: Parser Char
parseCharLit = do
            schar '\''
            c <- satisfy isPrint
            schar '\''
            return c

parseAt :: Parser ERHS
parseAt = do
        schar '@'
        return (ESimple SAnyChar)

parseHtext :: Parser String
parseHtext = munch (\c -> isPrint c  && c /= '}')
        



parsePreamble :: Parser String
parsePreamble = do
    p <- munch (/= '-')
    symbol "---\n"
    return p

-- reference from the last haskell lecture code
skipmore :: Parser ()
skipmore = do
    do 
        skipSpaces;
        optional (
            do
                string "--";
                endComment;
                skipmore
            )
endComment :: Parser ()
endComment = do
    char '\n'
    return ()
    <++ do
        get;
        endComment
        
    

-- Reference from Parsernotes
token :: Parser a -> Parser a
token p = skipmore >> p
symbol :: String -> Parser String
symbol = token . string
schar :: Char -> Parser Char
schar = token . char