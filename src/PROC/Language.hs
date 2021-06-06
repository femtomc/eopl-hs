{-# LANGUAGE MultiParamTypeClasses #-}

module PROC.Language where

import           Data.HashMap.Strict

-- This language includes let-bindings and procedures.
-- A procedure (or lambda) is an Expr
-- with a set of _formals_ and a _body_.
-- The body is an Expr which _captures_ the formals.
-- When the procedure is called -- it is lexically closed
-- from the surrounding environment.

data Identifier = String

-- interpret is now expanded to include the environment.
-- The environment is passed and updated with the interpreter.
type Env = HashMap String Int

-- An inductively defined data type which
-- represents the AST of a simple imperative language
-- with let bindings.
data Expr = LI Int
          | Add Expr Expr
          | Mult Expr Expr
          | CheckZero Expr
          | Let Identifier Expr Expr
          | If Expr Expr Expr
          | LambdaExpr [Identifier] Expr

data LambdaClosure = LambdaClosure { formals :: [Identifier], expr::Expr}

type ProcMap = HashMap String LambdaClosure

interpret :: Env -> ProcMap -> Expr -> Int
interpret e (LI a)           = a
interpret e (Add ex1 ex2)      = interpret e ex1 + interpret e ex2
interpret e (Mult ex1 ex2)     = interpret e ex1 * interpret e ex2
interpret e (CheckZero ex) = if 0 == interpret e ex
                                then 1
                                else 0
interpret e (If c ex1 ex2) = if 1 == interpret e c
                                  then interpret e ex1
                                  else interpret e ex2
interpret e (Let id ex1 ex2) = let nenv = insert id (interpret e ex1) e
                                in interpret nenv ex2
