-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP
import Data.Char

type ParseError = String -- you may replace this
type Parser a = ReadP a
parseString :: String -> Either ParseError Program
parseString s = case readP_to_S parseProgram s of
                [] -> Left "wrong"
                [(e,"")] -> Right e
                _  -> Left "implementation error"

--   Program ::= Stmts
parseProgram :: Parser Program
parseProgram = do 
    skipmore
    parseStmts


--   Stmts ::= Stmt';'Stmts | Stmt
parseStmts :: Parser [Stmt]
parseStmts =  do
        n <- parseStmt
        symbol ";"
        m <- parseStmts
        return $ n:m
    <++do 
        n <- parseStmt
        return [n]

--   Stmt ::= ident '=' Expr | Expr
parseStmt :: Parser Stmt
parseStmt =  do
        n <- parseIdent
        symbol "="
        SDef n <$> parseExpr 
    <++
        do SExp <$> parseExpr



reserved = ["True","False","None","for","if","in","not"]
parseIdent :: Parser String
parseIdent = cleara (do
    a <- satisfy (\x -> isLetter x ||  x =='_')
    b <- munch (\x -> isLetter x || isDigit x ||  x =='_')
    let n = a : b in
        if n `elem` reserved then pfail
        else return n)


parseNum::Parser Exp
parseNum = do
         char '-'
         d  <- satisfy (\v -> isDigit v && v /= '0')
         ds <- munch isDigit
         skipmore
         return (Const (IntVal (read ('-': d : ds) :: Int)))
    <++
      do d  <- satisfy (\v -> isDigit v && v /= '0')
         ds <- munch isDigit
         skipmore
         return (Const (IntVal (read (d : ds) :: Int)))
    <++
      do symbol  "-0"
         return (Const (IntVal 0))
    <++
      do char '0'
         skipmore
         return (Const (IntVal 0))


parsestring :: Parser Exp
parsestring = do
    char '\''
    s <- stringhelper ""
    return (Const (StringVal s))

stringhelper :: String -> Parser String
stringhelper s = 
    do
        char '\\'
        char '\n'
        stringhelper s    
    <++do
        char '\\'
        char '\\'
        stringhelper (s ++ "\\")
    <++
    do
        char '\\'
        char 'n'
        stringhelper (s ++ "\n")
    <++
    do
        char '\\'
        char '\''
        stringhelper (s ++ "'")
    <++
    do
        x <- satisfy (\a -> a /= '\'' && a /= '\\' && isPrint a && a /= '\n')
        stringhelper (s ++ [x])
    <++ do
        char '\''
        skipmore
        return s



--   Exprs ::= Expr | Expr ',' Exprs
parseExprs :: Parser [Exp]
parseExprs = do 
        e <- parseExpr
        symbol ","
        es <- parseExprs
        return (e:es) 
    <++
    do 
        e <- parseExpr
        return  [e]  

--   Exprz ::= Exprs | epsilon 
parseExprz :: Parser [Exp]
parseExprz = do 
        parseExprs
    <++ return []

--   Expr ::= 'not' Expr | Expr1
parseExpr :: Parser Exp
parseExpr  = do
        purestring "not"
        Not <$> parseExpr 
    <++
        parseExpr1 

--   Expr1 ::= Expr2 Operator1
parseExpr1 :: Parser Exp
parseExpr1  = do 
    x <- parseExpr2
    parseOperator1 x

-- Operator1 :: == != < <= > >= in not in epsilon
parseOperator1 :: Exp -> Parser Exp
parseOperator1 a = do
    symbol "=="
    Oper Eq a <$> parseExpr2
    <++ do
    symbol "!="
    Not . Oper Eq a <$> parseExpr2
    <++ do
    symbol "<"
    Oper Less a <$> parseExpr2
    <++ do
    symbol "<="
    Not . Oper Greater a <$> parseExpr2
    <++ do
    symbol ">"
    Oper Greater a <$> parseExpr2
    <++ do
    symbol ">="
    Not . Oper Less a <$> parseExpr2
    <++ do
    purestring "in"
    Oper In a <$> parseExpr2
    <++ do
    purestring "not"
    purestring "in"
    Not. Oper In a <$> parseExpr2
    <++
        return a

-- Expr2 ::= Expr3 Operator2
parseExpr2 :: Parser Exp
parseExpr2  = do 
    x <- parseExpr3
    parseOperator2  x

-- Operator2 :: + - 
parseOperator2 :: Exp -> Parser Exp
parseOperator2 x = do 
        symbol "+"
        a<- parseExpr3
        parseOperator2 (Oper Plus x a)
    <++
        do 
        symbol "-"
        a<- parseExpr3
        parseOperator2 (Oper Minus x a)
    <++
        return x

-- Expr3 ::= Term oper3
parseExpr3 :: Parser Exp
parseExpr3  = do 
    a <- parseTerm
    parseOperator3 a

-- Operator3 = * / %
parseOperator3 :: Exp -> Parser Exp
parseOperator3 x = do 
        symbol "*"
        a<- parseTerm
        parseOperator3 $ Oper Times x a 
    <++
        do 
        symbol "//"
        a<- parseTerm
        parseOperator3 $ Oper Div x a
    <++
        do 
        symbol "%"
        a<- parseTerm
        parseOperator3 $ Oper Mod x a
    <++
        return x

-- Term = numConst,stringConst,None,True,False,ident,(Expr),ident (Exprz),[Exprz],[Expr FroClause Clausez]
parseTerm:: Parser Exp
parseTerm =do 
    parseNum 
    <++ do 
        parseParnthes
    <++ do 
        e <- parseIdent 
        symbol "("
        x <- parseExprz
        symbol ")"
        return (Call e x)
    <++ do parseCompr
    <++ do parsesqParnthes
    <++ do Var <$> parseIdent
    <++ do parsestring
    <++ do parseTrue
    <++ do parseNone
    <++ do parseFalse

parseParnthes :: Parser Exp
parseParnthes = do
    symbol "("
    a <- parseExpr
    symbol ")"
    return a

parsesqParnthes :: Parser Exp
parsesqParnthes =  do
    symbol "["
    expz <- parseExprz
    symbol "]"
    return $ List expz

parseFor :: Parser [CClause]
parseFor = do
    purestring "for"
    name <- parseIdent
    purestring "in"
    e <- parseExpr
    return  [CCFor name e]

parseIf :: Parser [CClause]
parseIf = do
    purestring "if"
    e <- parseExpr
    return  [CCIf e]

parseCompr :: Parser Exp
parseCompr = do
    string "["
    skipmore
    e <- parseExpr
    for <- parseFor
    other <- parseClausez
    symbol "]"
    return $ Compr e (for ++ other)

--   Clausez ::= ε | ForClause Clausez | IfClause Clausez
parseClausez:: Parser [CClause]
parseClausez= do 
        e <- parseFor
        es <- parseClausez
        return $ e ++ es 
    <++do 
        e <- parseIf
        es <- parseClausez
        return $ e ++ es 
    <++do 
        skipmore
        return []


parseNone::Parser Exp
parseNone = do 
    string "None"
    skipmore
    return $ Const NoneVal

parseTrue::Parser Exp
parseTrue = do 
    symbol "True"
    return $ Const TrueVal

parseFalse::Parser Exp
parseFalse = do 
    symbol "False"
    return $ Const FalseVal




symbol:: String ->Parser ()
symbol s = do
    string s
    skipmore
    return ()


purestring:: String ->Parser ()
purestring s =cleara (do
    string s
    a <-look
    if head a == '['
        || head a == '('
        || head a == '#'
        || isSpace (head a)
    then return ()
    else return pfail "need parentheses or space!")
            

skipmore :: Parser ()
skipmore = do
    skipSpaces
    s <- look
    if s == "" then return ()
            else if s /= "" && head s == '#' 
                then do munch1 (/= '\n'); skipmore
                else do skipSpaces; return ()

cleara:: Parser a -> Parser a
cleara p = do
    a <- p
    skipmore
    return a

