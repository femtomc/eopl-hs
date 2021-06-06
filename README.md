# eopl-hs

Haskell implementations of the interpreters from [Essentials of Programming Languages](https://www.amazon.com/Essentials-Programming-Languages-MIT-Press/dp/0262062798).

## Description

Each subdirectory of `/src` contains a `Language.hs` implementation file which defines the language, any associated data types, and a corresponding `interpret` function.

The language data types are typically `Expr` -- just a raw AST (without a parser).
