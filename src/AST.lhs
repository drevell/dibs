A bunch of types and typeclasses that represent an abstract syntax tree for
the HDB query language.

> module AST where
> import Control.Concurrent.STM
> import Schema

> data HDBValue = HDBInt Int | HDBChar Char | HDBString String | HDBBool Bool 
>                | HDBMulti [[HDBValue]]

> class HDB a where
>     run :: Schema -> a -> STM HDBValue
>
>-- instance Monad HDB where
>--       t1 >>= t2  =  t2 $ run t1
>--

A type representing the result of evaluating an expression or statement.

>-- instance HDB HDBValue where



A type class for Haskell types that can be used as HDB values. There is a wrap
function for each type that wraps the value in an HDBPrim type.

> class HDBPrim a where
>       wrap :: a -> HDBValue
> instance HDBPrim Int where
>       wrap x = HDBInt x
> instance HDBPrim Char where
>       wrap x = HDBChar x
>-- instance HDBPrim [ where
>--       wrap x = HDBString x
> instance HDBPrim Bool where
>       wrap x = HDBBool x

> data HDBCreate = HDBCreate String [(String, ColType)]
> instance HDB HDBCreate where
>       run schema astNode = do
>           let HDBCreate tableName colNames = astNode
>           createTable schema tableName colNames -- could throw exception
>           return (HDBBool True)
>
> {-
> data HDBGet = MkHDBGet String [String] HDBCond
> instance HDB HDBGet where
>       run astNode = do
>           let MkHDBGet tableName colNames cond = astNode
>           tableTv <- lookupTable tableName
>           columnTvs <- lookupColumns tableTv colNames
>           condResolved <- lookupCondCols cond  -- resolve column names to tvars
>           resultIds <- rowsMatching cond condColTvs
>           resultList <- makeResults resultIds columnTvs
>           return resultList
> -}