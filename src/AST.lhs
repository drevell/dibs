A bunch of types and typeclasses that represent an abstract syntax tree for
the Dibs transaction language.

There has to be some better way to express the "run" function, instead of as a
series of guards. This could probably use type classes in some elegant way 
that I don't yet understand.

> module AST where
> import Control.Concurrent.STM
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Schema

>-- class DibsAST a where
>--     run :: Schema -> a -> STM DibsValue

> data (Conditional c) => DibsAST c = DibsCreate String [(String, ColType)]
>              | DibsGet String [String] c
>              | Equals DibsValue DibsValue
>              | LiteralTrue
>
> class Conditional c where
>     eval :: c -> MultiValue -> Bool
>     resolveColumns :: c -> [(String, Int)] -> c
> instance Conditional LiteralTrue where
>     eval c v = True
>     resolveColumns x = x
> instance Conditional LiteralFalse where
>     eval c v = False
>     resolveColumns x = x
>
> data SingleValue = DibsInt64 Int | DibsChar Char | DibsString String 
>                  | DibsBool Bool 
>       deriving (Show, Eq)

A type representing the result of evaluating an expression or statement.

>-- instance Dibs DibsValue where



> run :: Schema -> DibsAST -> STM [MultiValue]
> run schema (DibsCreate tableName columns) = do
>           createTable schema tableName columns -- could throw exception
>           return (DibsBool True)
> run schema (DibsGet tableName colNames cond) = do
>           table <- getTableByName schema tableName
>           columns <- getColumnsByName table colNames
>           multis <- columnsToMultis columns
>           newMultis <- condFilter cond
>           return newMultis

Take some multivalues representing an intermediate result with some number of
rows and columns, and filter the rows according to the given conditional.

> condFilter :: [MultiValue] -> Conditional -> [MultiValue]
> condFilter inMultis cond =  -- wrapper that makes empty maps for accumulator
>       condFilter' (emptyMaps numMultis) inMultis cond
>      where
>       numMultis = length inMultis
>       emptyMaps n = take n (repeat Map.empty)
> condFilter' :: [MultiValue] -> [MultiValue] -> Conditional -> [MultiValue]
> condFilter' accs inMultis cond = 
>       --extract name,col# pairs from multis
>       --resolve cond into resolvedCond using names
>       let (id, values) = multiHead inMultis in
>       case eval row of
>         True ->
>           condFil
>      where
>       extractMap = 
>       multiHead = extract 
>       --flatten each tree into l1..ln
>       --take the head of each l1..ln into a list "row"
>       --pass "row" to the eval function with resolvedCond
>       --

Wrap a single value in a multivalue table-like structure. The map key 0 is used
because map keys are not useful in this context. They're only used for matching
values in different maps that correspond to the same row in a table. Since the
boxed value that we create here will never be used in that context, we set the
key to zero as a default. The table and column names are "_" because this data
did not derive from a table, yet still requires a value for those fields.

> boxAsMulti :: SingleValue -> MultiValue
> boxAsMulti (DibsInt64 i)  = MultiInt64  "_" "_" $ Map.insert 0 i Map.empty
> boxAsMulti (DibsChar c)   = MultiChar   "_" "_" $ Map.insert 0 c Map.empty
> boxAsMulti (DibsBool b)   = MultiBool   "_" "_" $ Map.insert 0 b Map.empty
> boxAsMulti (DibsString s) = MultiString "_" "_" $ Map.insert 0 s Map.empty

> mvHead :: MultiValue -> DibsValue
> mvHead MultiString String String