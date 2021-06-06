{-# LANGUAGE MultiParamTypeClasses #-}

module LET.Language where

import           Data.HashMap.Strict

data Identifier = Id String

get_string :: Identifier -> String
get_string (Id a) = a

-- interpret is now expanded to include the environment.
-- The environment is passed and updated with the interpreter.
newtype Env = Env (HashMap String Int)

unwrap :: Env -> HashMap String Int
unwrap (Env a) = a

-- An inductively defined data type which
-- represents the AST of a simple imperative language
-- with let bindings.
data Expr = LI Int
          | Add Expr Expr
          | Mult Expr Expr
          | CheckZero Expr
          | Let Identifier Expr Expr
          | IfExpr Expr Expr Expr

interpret :: Env -> Expr -> Int
interpret e (LI a)           = a
interpret e (Add ex1 ex2)      = interpret e ex1 + interpret e ex2
interpret e (Mult ex1 ex2)     = interpret e ex1 * interpret e ex2
interpret e (CheckZero ex) = if 0 == interpret e ex
                                then 1
                                else 0
interpret e (IfExpr c ex1 ex2) = if 1 == interpret e c
                                  then interpret e ex1
                                  else interpret e ex2
interpret e (Let id ex1 ex2) = let nenv = Env (insert (get_string id) (interpret e ex1) (unwrap e))
                                in interpret nenv ex2
