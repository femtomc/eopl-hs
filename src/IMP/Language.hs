{-# LANGUAGE MultiParamTypeClasses #-}

module IMP.Language where

-- An inductively defined data type which
-- represents the AST of a simple imperative language
data Expr = LI Int
          | Add Expr Expr
          | Mult Expr Expr
          | CheckZero Expr
          | IfExpr Expr Expr Expr

interpret :: Expr -> Int
interpret (LI a)           = a
interpret (Add e1 e2)      = interpret e1 + interpret e2
interpret (Mult e1 e2)     = interpret e1 * interpret e2
interpret (CheckZero expr) = if 0 == (interpret expr :: Int) then 1 else 0
interpret (IfExpr c e1 e2) = if (1 == (interpret c :: Int)) then interpret e1 else interpret e2
