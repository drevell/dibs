
> module Parser where

> import AST
> import Schema

Turns an input string into a parsed abstract syntax tree.

> parse :: Dibs a => String -> Dibs
> parse s = DibsCreate "mytable" [("mycol", Int64ColType)]
