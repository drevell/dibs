
> module Parser where

> import AST
> import Schema

Turns an input string into a parsed abstract syntax tree.

> parse :: String -> (DibsAST, String)
> parse s = 
>
> exampleCreate :: String -> (DibsAST, String)
> exampleCreate = (DibsCreate "mytable" [("mycol", Int64ColType)], "")
>
> exampleGet :: String -> (DibsAST, String)
> exampleGet = (DibsGet "mytable" ["mycol"] LiteralTrue, "")
