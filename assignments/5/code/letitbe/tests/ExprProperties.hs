module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified Data.Map.Strict as M

isVarElem ::Expr->Bool
isVarElem (Const _)=False
isVarElem (Var _)=True
isVarElem (Oper _ e1 e2)= isVarElem e1 || isVarElem e2
isVarElem (Let _ e1 e2)=isVarElem e1 || isVarElem e2

exprn :: E.Env -> Int -> Gen Expr
exprn env 0 = case M.keys env of
                [] -> Const <$> arbitrary
                vs -> oneof [Const <$> arbitrary, Var <$> elements vs]
exprn env n = case M.keys env of
                [] -> oneof [
                  Const <$> arbitrary, 
                  Oper Plus <$> exprn env (n`div`2) <*> exprn env (n`div`2),
                  Oper Minus <$> exprn env (n`div`2) <*> exprn env (n`div`2),
                  Oper Times <$> exprn env (n`div`2) <*> exprn env (n`div`2),
                  Let <$> arbitrary <*> exprn env (n`div`2) <*> exprn env (n`div`2)
                  ] 
                vs -> oneof [
                  Const <$> arbitrary,
                  Var <$> elements vs, 
                  Oper Plus <$> exprn env (n`div`2) <*> exprn env (n`div`2),
                  Oper Minus <$> exprn env (n`div`2) <*> exprn env (n`div`2),
                  Oper Times <$> exprn env (n`div`2) <*> exprn env (n`div`2),
                  Let <$> arbitrary <*> exprn env (n`div`2) <*> exprn env (n`div`2)
                  ]
  
  
  -- oneof [Const <$> arbitrary,
  --                   Var <$> elements (M.keys env),
  --                   Oper Plus <$> exprn env (n `div` 2) <*> exprn env (n `div` 2),
  --                   Oper Minus <$> exprn env (n `div` 2) <*> exprn env (n `div` 2),
  --                   Oper Times <$> exprn env (n `div` 2) <*> exprn env (n `div` 2),
  --                   Let <$> listOf arbitrary <*> (exprn env (n `div` 2) `suchThat` (not . isVarElem))  <*> exprn env (n `div` 2)]

expr:: Int -> Gen Expr
expr = exprn M.empty
  -- where
  --   expr' 0 = fmap Const arbitrary
  --   expr' n =
  --     oneof [fmap Const arbitrary
  --           , fmap Var arbitrary
  --           , Oper Plus <$> expr' (n `div` 2) <*> expr' (n `div` 2)
  --           , Oper Minus <$> expr' (n `div` 2) <*> expr' (n `div` 2)
  --           , Oper Times <$> expr' (n `div` 2) <*> expr' (n `div` 2)
  --           , Let <$> listOf arbitrary <*> (expr' (n `div` 2) `suchThat` (not . isVarElem)) <*> expr' (n `div` 2)
  --           ]


instance Arbitrary Expr where
  arbitrary = sized expr

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.evalTop x === E.evalTop (E.simplify x)
