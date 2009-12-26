
> module Parser where

Lots of the code in this module is based on material from the Parsec user's
manual at http://legacy.cs.uu.nl/daan/download/parsec/parsec.html .

This module will be very difficult to understand without a good understanding
of Parsec (or monadic parser combinators in general, I suppose). Check out the
Parsec documentation.

TODO: left-factor the DQL grammar for performance (probably will reduce clarity)

> import AST
> import Schema
> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import Data.Map (Map)
> import Data.Map as Map
> import qualified Text.ParserCombinators.Parsec.Token as PT
> import Text.ParserCombinators.Parsec.Language
> import Util
> import Monad
> import ValueTypes

Turns an input string into a parsed abstract syntax tree. Since the Parsec
parser will give a list of DibsAST nodes, we join them into a tree by folding
the DibsSeq expression sequence node across the list.

> dqlParse :: String -> Either String DibsAST
> dqlParse s = case parse exprListParser "" s of
>             Left err -> Left (show err)
>             Right astList -> Right $ Prelude.foldr1 DibsSeq astList

Temporary, parse only one expression.

> dqlSingleParse :: String -> Either String DibsAST
> dqlSingleParse s = case parse expr "" s of
>             Left err -> Left (show err)
>             Right parsed -> Right parsed


Functions to efficiently reference different parsers for various lexemes. Each
of these functions uses the Parsec-generated lexer to consume and return a
token, and also consume and discard its trailing whitespace.

> whiteSpace = PT.whiteSpace lexer
> semi       = PT.semi lexer
> identifier = PT.identifier lexer
> reserved   = PT.reserved lexer
> reservedOp = PT.reservedOp lexer
> parens     = PT.parens lexer  
> natural    = PT.natural lexer
> squares    = PT.squares lexer
> comma      = PT.comma lexer
> commaSep1  = PT.commaSep1 lexer
> symbol     = PT.symbol lexer
> stringLiteral = PT.stringLiteral lexer

Parse a string into a list of expressions, which correspond to the semicolon-
separated expressions in the input.

> exprListParser :: Parser [DibsAST]
> exprListParser = do
>       whiteSpace  -- parser automatically discards trailing (not leading) w.s.
>       many1 (do {e <- expr ; semi ; return e})

> dqlOperators = ["+", "*", "/", "-"]
> dqlReserved = ["create", "get", "insert"]

Create a lexer using Haskell standards for whitespace, identifiers, etc.

> lexer :: PT.TokenParser ()
> lexer = PT.makeTokenParser $ haskellDef { reservedOpNames = dqlOperators,
>                                           reservedNames = dqlReserved}

> factor :: Parser DibsAST
> factor = parens expr 
>        <|> literalP
>        <?> "factor expr"

> literalP :: Parser DibsAST
> literalP = do
>         n <- natural
>         return $ DibsLiteral . DibsInt64 . fromIntegral $ n 


> arithOperTable = 
>       let makeOp s astNode = Infix (do 
>                                       reservedOp s
>                                       return astNode) AssocLeft
>       in
>       [[makeOp "*" DibsMultiply, makeOp "/" DibsDivide]
>       ,[makeOp "+" DibsAdd, makeOp "-" DibsSubtract]]


Stuff from Parsec documentation. It looks like the expressionParser is not 
adequate for our purposes.

> arithP :: Parser DibsAST
> arithP = buildExpressionParser arithOperTable factor <?> "expression"

Parses an identifier (according to the Parsec lexer) and returns it as a 
DibsIdentifier

> identP :: Parser DibsAST
> identP = do
>       s <- identifier
>       return $ DibsIdentifier s

Parses a string literal, i.e. with double quotes

> stringP :: Parser DibsAST
> stringP = do
>       str <- stringLiteral
>       return $ DibsLiteral . DibsString $ str

A parser for built-in DQL functions. Goes through the list of possible functions
and tries to parse each one.

> dqlFunc :: Parser DibsAST
> dqlFunc = (foldl1 (<|>) (Prelude.map try dqlFuncParsers))
> dqlFuncParsers = [dqlGetP, dqlInsertP, dqlCreateP] -- ordered by likelihood

Parsers for the various possible DQL functions.
TODO: Factor out code, define each parser as a data structure

> dqlCreateP, dqlGetP, dqlInsertP :: Parser DibsAST
> dqlCreateP = do 
>       reserved "create"
>       argTuple <- tupleP [stringP, listP stringP]
>       let DibsTuple args = argTuple
>       return $ applyList2 DibsCreate args
> dqlGetP = do
>       reserved "get"
>       argTuple <- tupleP [stringP, listP stringP]
>       let DibsTuple args = argTuple
>       return $ applyList2 DibsGet args
> dqlInsertP = do
>       reserved "insert"
>       argTuple <- tupleP [stringP, listP stringP, listP (listP literalP)]
>       let DibsTuple args = argTuple
>       return $ applyList3 DibsInsert args

Parse a tuple, where the type of each element is given by the list argument.
Since the tuple being parsed may have any size, we return the result as a list.

> tupleP :: [Parser DibsAST] -> Parser DibsAST
> tupleP ps = do  -- TODO: fold?
>       symbol "("
>       vs <- parseEntries [] ps
>       return $ DibsTuple vs
>      where
>       parseEntries :: [DibsAST] -> [Parser DibsAST] -> Parser [DibsAST]
>       parseEntries acc (p:ps) = do
>               v <- p
>               let newAcc = (v:acc)
>               -- Either parse a comma and recurse, or parse ')' and stop
>               (<|>) (comma >> parseEntries newAcc ps) 
>                     (symbol ")" >> return (reverse newAcc))

> listP :: Parser DibsAST -> Parser DibsAST
> listP p = do
>       exprs <- squares $ commaSep1 p :: Parser [DibsAST]
>       return $ DibsList exprs

> expr :: Parser DibsAST
> expr = try (parens expr)
>    <|> try dqlFunc
>    <|> try arithP
>    <?> "expression" 

Fake parsing code for avoiding Parsec and doing simple tests

> fakeParse s = case (head s) of
>             'c' -> Right $ exampleCreate s
>             'g' -> Right $ exampleGet s
>             'i' -> Right $ exampleInsert s
>             _   -> Left  $ ("Unrecognized fake parse cmd: " ++ s)
>
> exampleCreate :: String -> Either String DibsAST
> exampleCreate s = let tablename = DibsLiteral $ DibsString "mytable"
>                       colnames = DibsList [DibsLiteral $ DibsString "mycol"]
>                   in Right $ DibsCreate tablename colnames
>
> exampleGet :: String -> Either String DibsAST
> exampleGet s = let tablename = DibsLiteral $ DibsString "mytable"
>                    colnames = DibsList [DibsLiteral $ DibsString "mycol"]
>                in Right $ DibsGet tablename colnames
>
> exampleInsert :: String -> Either String DibsAST
> exampleInsert s = let tablename = DibsLiteral $ DibsString "mytable"
>                       colnames = DibsList [DibsLiteral $ DibsString "mycol"]
>                       values = DibsList $ [DibsList [DibsLiteral $ DibsInt64 13]]
>                   in Right $ DibsInsert tablename colnames values
