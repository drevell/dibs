
> module Parser where

> import AST
> import Schema

Turns an input string into a parsed abstract syntax tree.

> parse :: String -> Either String (DibsAST, String)
> parse s = fakeParse s 

> fakeParse s = Right $ case (head s) of
>             'c' -> exampleCreate s
>             'g' -> exampleGet s
>             'i' -> exampleInsert s
>
> exampleCreate :: String -> (DibsAST, String)
> exampleCreate s = (DibsCreate "mytable" [("mycol", Int64ColType)], "")
>
> exampleGet :: String -> (DibsAST, String)
> exampleGet s = (DibsGet "mytable" ["mycol"], "")
>
> exampleInsert :: String -> (DibsAST, String)
> exampleInsert s = (DibsInsert "mytable" ["mycol"] [[DibsInt64 13]], "")
