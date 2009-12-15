A bunch of types and typeclasses that represent an abstract syntax tree for
the Dibs query language.

> module AST where
> import Control.Concurrent.STM
> import Schema

> data DibsValue = DibsInt Int | DibsChar Char | DibsString String | DibsBool Bool 
>                | DibsMulti [[DibsValue]]

> class Dibs a where
>     run :: Schema -> a -> STM DibsValue
>
>-- instance Monad Dibs where
>--       t1 >>= t2  =  t2 $ run t1
>--

A type representing the result of evaluating an expression or statement.

>-- instance Dibs DibsValue where



A type class for Haskell types that can be used as Dibs values. There is a wrap
function for each type that wraps the value in an DibsPrim type.

> class DibsPrim a where
>       wrap :: a -> DibsValue
> instance DibsPrim Int where
>       wrap x = DibsInt x
> instance DibsPrim Char where
>       wrap x = DibsChar x
>-- instance DibsPrim [ where
>--       wrap x = DibsString x
> instance DibsPrim Bool where
>       wrap x = DibsBool x

> data DibsCreate = DibsCreate String [(String, ColType)]
> instance Dibs DibsCreate where
>       run schema astNode = do
>           let DibsCreate tableName colNames = astNode
>           createTable schema tableName colNames -- could throw exception
>           return (DibsBool True)
>
> {-
> data DibsGet = MkDibsGet String [String] DibsCond
> instance Dibs DibsGet where
>       run astNode = do
>           let MkDibsGet tableName colNames cond = astNode
>           tableTv <- lookupTable tableName
>           columnTvs <- lookupColumns tableTv colNames
>           condResolved <- lookupCondCols cond  -- resolve column names to tvars
>           resultIds <- rowsMatching cond condColTvs
>           resultList <- makeResults resultIds columnTvs
>           return resultList
> -}
