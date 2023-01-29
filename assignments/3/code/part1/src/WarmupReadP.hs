module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E ::= T E' | "-" T E'.
--   E'::= "+" T E' | "-" T E' | \epsilon
--   T ::= num | "(" E ")".

import Text.ParserCombinators.ReadP
-- import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec
import Data.Char
-- import GHC (XExpBr)

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseProgram :: ReadP Exp
parseProgram = do
  skipSpaces
  parseE

parseI :: Parser Exp
parseI = do
  d <- satisfy (\v -> isDigit v && v /= '0')
  ds <- munch isDigit
  return (Num (read (d:ds) :: Int))

parsenum:: ReadP Exp
parsenum = do 
    e <- parseI
    skipSpaces
    return e
    <++
    do
      char '0'
      skipSpaces
      return (Num 0)
        
parseParenthes:: ReadP Exp
parseParenthes = do
  skipSpaces
  s <- string "("
  skipSpaces
  e <- parseE
  skipSpaces
  s <- string ")"
  skipSpaces
  return e

parseT:: ReadP Exp
parseT = do
  parsenum 
  <++ parseParenthes

parseE'::Exp -> ReadP Exp
parseE' e = do
  char '+'
  skipSpaces
  t <- parseT
  parseE' (Add e t)
  <++ do
    string "-"
    skipSpaces
    t <- parseT
    parseE' (Add e (Negate t))
  <++ do
    skipSpaces
    return e

parseE:: ReadP Exp
parseE = do
  t <- parseT
  parseE' t
  <++ do
    string "-"
    skipSpaces
    t <- parseT
    parseE' (Negate t)
  

parseString :: String -> Either ParseError Exp
parseString s
  | null s = Left "empty!"
  | otherwise = case readP_to_S parseProgram s of
    [] -> Left "empty list"
    a -> case a of
      [(ans,"")] -> Right ans
      _ -> Left "parse err"
