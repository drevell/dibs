
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
>   Left err -> Left (show err)
>   Right astList -> Right $ Prelude.foldr1 (\x y -> DibsSeq [x,y]) astList


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
> charLiteral = PT.charLiteral lexer
> naturalOrFloat = PT.naturalOrFloat lexer 

Parse a string into a list of expressions, which correspond to the semicolon-
separated expressions in the input.

> exprListParser :: Parser [DibsAST]
> exprListParser = do
>       whiteSpace  -- parser automatically discards trailing (not leading) w.s.
>       many1 (do {e <- expr ; semi ; return e})

> dqlOperators = ["+", "*", "/", "-", "==", "!="]
> dqlReserved = ["create", "get", "cond_get", "insert", "true", "false", 
>       "all"]

Create a lexer using Haskell standards for whitespace, identifiers, etc.

> lexer :: PT.TokenParser ()
> lexer = PT.makeTokenParser $ haskellDef { reservedOpNames = dqlOperators,
>                                           reservedNames = dqlReserved}

> literalP = choice $ Prelude.map try 
>       [numLiteralP, stringLiteralP, boolLiteralP, charLiteralP]
> numLiteralP = do
>   eitherIntFloat <- naturalOrFloat
>   return $ DibsLiteral $ case eitherIntFloat of
>       Left i -> DibsInt64 $ fromIntegral i
>       Right f -> DibsDouble f
> stringLiteralP = do
>       str <- stringLiteral
>       return $ DibsLiteral . DibsString $ str
> boolLiteralP = do -- parse a "true" or "false" (but without quotes)
>    boolVal <- choice $ Prelude.map try [ do {reserved "true" ; return True}
>                                        , do {reserved "false" ; return False}] 
>    return $ DibsLiteral . DibsBool $ boolVal
> charLiteralP = do
>    c <- charLiteral
>    return $ DibsLiteral . DibsChar $ c

Parses an identifier (according to the Parsec lexer) and returns it as a 
DibsIdentifier

> identP :: Parser DibsAST
> identP = do
>       s <- identifier
>       return $ DibsIdentifier s

To specify a list of columns to retrieve when doing a "get" or "cond_get", the
client will give either an expression that evaluates to a list of column names,
or the token "all".

> colSpecP = try (reserved "all" >> return DibsAllColumns)
>       <|> expr


A parser for built-in DQL functions. Goes through the list of possible functions
and tries to parse each one.

> dqlFunc :: Parser DibsAST
> dqlFunc = choice $ Prelude.map try dqlFuncParsers
> dqlFuncParsers = Prelude.map makeParser [
>   ("create", DibsCreate, [stringLiteralP, listP stringLiteralP]),
>   ("get", DibsGet, [stringLiteralP, colSpecP]),
>   ("cond_get", DibsCondGet, [stringLiteralP, colSpecP, expr]),
>   ("insert", DibsInsert, [stringLiteralP, 
>                           listP stringLiteralP, listP (listP literalP)])]
>  where
>   res = reserved
>   op = reservedOp
>   makeParser (str, constructor, argList) = do
>       reserved str
>       DibsTuple args <- tupleP argList
>       return $ constructor args

Parse a tuple, where the type of each element is given by the list argument.
Since the tuple being parsed may have any size, we return the result as a list.

> tupleP :: [Parser DibsAST] -> Parser DibsAST
> tupleP ps = do
>       symbol "("
>       vs <- parseEntries ps
>       return $ DibsTuple vs
>      where
>       parseEntries :: [Parser DibsAST] -> Parser [DibsAST]
>       parseEntries (p:ps) = do
>               v <- p
>               -- Either parse a comma and recurse, or parse ')' and stop
>               (<|>) (do comma
>                         vs <- parseEntries ps
>                         return (v:vs)) 
>                     (do symbol ")"
>                         case ps of
>                             [] -> return [v]
>                             _  -> fail "Not enough arguments")
>       parseEntries [] = fail "Too many arguments"

> listP :: Parser DibsAST -> Parser DibsAST
> listP p = do
>       exprs <- squares $ commaSep1 p :: Parser [DibsAST]
>       return $ DibsList exprs

> expr :: Parser DibsAST
> expr = try infixExpr <|> try exprNoInfix
> exprNoInfix = try (parens expr)
>    <|> try dqlFunc
>    <|> try identP
>    <|> try literalP
>    <?> "expression" 

To parse infix expressions, we use Parsec's built-in expression parser which
automatically generates a left-factored grammar that works around issues with
recursive grammars.

> infixExpr :: Parser DibsAST
> infixExpr = buildExpressionParser infixOps infixTerm
> infixOps = [[Infix times AssocLeft, Infix divide AssocLeft], 
>             [Infix plus AssocLeft, Infix minus AssocLeft],
>             [Infix equals AssocLeft, Infix notEquals AssocLeft,
>              Infix lessThan AssocLeft, Infix lessThanOrEqual AssocLeft, 
>              Infix greaterThan AssocLeft, Infix greaterOrEqual AssocLeft]] 
>                
> infixTerm = exprNoInfix -- have to prevent recursion, no infix allowed
> times = reservedOp "*" >> return (listCurry DibsMultiply)
> plus = reservedOp "+" >> return (listCurry DibsAdd)
> minus = reservedOp "-" >> return (listCurry DibsSubtract)
> divide = reservedOp "/" >> return (listCurry DibsDivide)
> equals = reservedOp "==" >> return (listCurry DibsEq)
> notEquals = reservedOp "!=" >> return (listCurry DibsNotEq)
> lessThan = reservedOp "<" >> return (listCurry DibsLessThan)
> lessThanOrEqual = reservedOp "<=" >> return (listCurry DibsLessThanOrEq)
> greaterThan = reservedOp ">" >> return (listCurry DibsGreaterThan)
> greaterOrEqual = reservedOp ">=" >> return (listCurry DibsGreaterThanOrEq)

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
>                   in Right $ DibsCreate [tablename, colnames]
>
> exampleGet :: String -> Either String DibsAST
> exampleGet s = let tablename = DibsLiteral $ DibsString "mytable"
>                    colnames = DibsList [DibsLiteral $ DibsString "mycol"]
>                in Right $ DibsGet [tablename, colnames]
>
> exampleInsert :: String -> Either String DibsAST
> exampleInsert s = let tablename = DibsLiteral $ DibsString "mytable"
>                       colnames = DibsList [DibsLiteral $ DibsString "mycol"]
>                       values = DibsList $ [DibsList [DibsLiteral $ DibsInt64 13]]
>                   in Right $ DibsInsert [tablename, colnames, values]
