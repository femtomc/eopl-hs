{-# LANGUAGE MultiParamTypeClasses #-}

module LET.Language where

import           Data.HashMap.Strict

type Identifier = String

-- interpret is now expanded to include the environment.
-- The environment is passed and updated with the interpreter.
type Env = HashMap String Int

init_env :: HashMap Identifier Int
init_env = empty :: HashMap Identifier Int

extend_env :: Env -> Identifier -> Int -> Env
extend_env e id v = insert id v e

get :: Env -> Identifier -> Int
get e id = (!) e id

-- An inductively defined data type which
-- represents the AST of a simple imperative language
-- with let bindings.
data Expr = LI Int
          | LId Identifier
          | Add Expr Expr
          | Mult Expr Expr
          | CheckZero Expr
          | Let Identifier Expr Expr
          | IfExpr Expr Expr Expr

_interpret :: Env -> Expr -> Int
_interpret e (LI a)             = a
_interpret e (LId id)           = get e id
_interpret e (Add ex1 ex2)      = interpret e ex1 + interpret e ex2
_interpret e (Mult ex1 ex2)     = interpret e ex1 * interpret e ex2
_interpret e (CheckZero ex)     = if 0 == interpret e ex
                                then 1
                                else 0
_interpret e (IfExpr c ex1 ex2) = if 1 == interpret e c
                                  then interpret e ex1
                                  else interpret e ex2
_interpret e (Let id ex1 ex2)   = let nenv = extend_env e id (interpret e ex1)
                                in interpret nenv ex2

interpret :: Expr -> Int
interpret ex = _interpret init_env ex
