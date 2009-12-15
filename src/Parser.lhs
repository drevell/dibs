
> module Parser where

> import AST
> import Schema

Turns an input string into a parsed abstract syntax tree.

> parse :: HDB a => String -> HDB
> parse s = HDBCreate "mytable" [("mycol", Int64ColType)]