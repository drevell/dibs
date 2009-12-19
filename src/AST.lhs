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
> import List

>-- class DibsAST a where
>--     run :: Schema -> a -> STM DibsValue

> data DibsAST = DibsCreate String [(String, ColType)]
>              | DibsGet String [String]
>              | DibsInsert String [String] [[SingleValue]]
>--              | Equals DibsValue DibsValue
>--              | LiteralTrue
>
>-- class Conditional c where
>--     eval :: c -> MultiValue -> Bool
>--     resolveColumns :: c -> [(String, Int)] -> c
>-- instance Conditional LiteralTrue where
>--     eval c v = True
>--     resolveColumns x = x
>-- instance Conditional LiteralFalse where
>--     eval c v = False
>--     resolveColumns x = x

Implementations of the behavior for each Dibs operator.

> run :: Schema -> DibsAST -> STM [MultiValue]
> run schema (DibsCreate tableName columns) = do
>           createTable schema tableName columns -- could throw exception
>           return $ [boxAsMulti (wrap True)]
> run schema (DibsGet tableName colNames) = do
>           table <- getTableByName schema tableName
>           columns <- getColumnsByName table colNames
>           multis <- columnsToMultis columns
>           return multis
>           --newMultis <- condFilter cond
>--           return newMultis
> run schema (DibsInsert tableName names rows) = do
>       table <- getTableByName schema tableName
>       columns <- getColumnsByName table names
>       oldMultis <- columnsToMultis columns
>       nextID <- readTVar $ tNextID table
>       let insertMultis = rowsToMulti rows (nextID)
>--       let colToNameMap = buildColNameMap names
>--       let nameToColMap = buildNameColMap oldMultis
>--       let permutation = [mapLook nameToColMap . mapLook colToNameMap $ y 
>--                      | y <- [0..(length names)]]
>--       let orderedInsertMultis = permute permutation insertMultis
>--       let finalMultis = mvListUnion oldMultis orderedInsertMultis
>       let finalMultis = mvListUnion oldMultis insertMultis
>       -- write each multivalue back to its column tvar
>       mapM (\(c,mv) -> writeTVar (colMultiVal c) mv) $ zip columns finalMultis
>       writeTVar (tNextID table) $ nextID + (fromIntegral (length rows))
>       return insertMultis -- return stored values for use in expression
>--      where
>--       mapLook :: (Ord k) => Map k a -> k -> a  -- wrapper to deal with Monad
>--       mapLook m k = case Map.lookup k m of
>--               Just x -> x
>--               Nothing -> error "Unexpected missing key in DibsInsert"
>--       permute permutation l =  [l !! x | x <- permutation]
>--       namesToMultis (name:names) multis = 
>--         let tree = Map.nameToMulti name ++ namesToMultis names

Compute unions of MultiValues and lists of MultiValues

> mvUnion :: MultiValue -> MultiValue -> MultiValue
> mvUnion mv1 mv2 = 
>       let MultiValue tableName colName _ = mv1 -- take titles from 1st arg
>       in MultiValue tableName colName $ Map.union (mValMap mv1) (mValMap mv2)
> mvListUnion :: [MultiValue] -> [MultiValue] -> [MultiValue]
> mvListUnion [] [] = []
> mvListUnion [] _  = error "First list was longer, in mvListUnion"
> mvListUnion _ []  = error "Second list was longer, in mvListUnion" 
> mvListUnion (h1:t1) (h2:t2) = mvUnion h1 h2 : mvListUnion t1 t2

> -- convert from a row-major list of rows to a column-major list of multivalues
> rowsToMulti :: [[SingleValue]] -> ID -> [MultiValue]
> rowsToMulti rowLists startID = 
>       let rowWidth = length $ head rowLists
>           startAccum = take rowWidth (repeat [])
>           colLists = transpose rowLists 
>       in
>       [MultiValue "_" "_" (Map.fromList (zip [startID] colList)) | colList <- colLists]
>--       let rowWidth = length (head rows) -- assume rows are all same width
>--           startAccum = (take rowWidth (repeat Map.empty)) in
>--       [MultiValue "_" "_" m | m <- rowsToMulti' startAccum rows startID]
>--      where 
>--       insert3Tuple :: (Map ID SingleValue, ID, SingleValue) -> Map ID SingleValue
>--       insert3Tuple (m, k, v) = Map.insert k v m
>--       rowsToMulti' :: [Map ID SingleValue] -> [[SingleValue]] -> ID -> [Map ID SingleValue]
>--       rowsToMulti' accum [] _ = accum
>--       rowsToMulti' accum (r:rs) key = 
>--               let newAccum = map insert3Tuple (zip3 accum key r) in
>--               rowsToMulti' newAccum rs (key+1)


>-- buildColNameMap :: [String] -> Map Int String
>-- buildColNameMap names = 
>--       let pairList = zip names [0..] in 
>--       Map.fromList pairList

>-- buildNameColMap :: [MultiValue] -> Map String Int
>-- buildNameColMap acc columns =
>--       let pairList = zip (map extractName columns) [0..] in
>--       Map.fromList pairList

Take some multivalues representing an intermediate result with some number of
rows and columns, and filter the rows according to the given conditional.

>-- condFilter :: [MultiValue] -> Conditional -> [MultiValue]
>-- condFilter inMultis cond =  -- wrapper that makes empty maps for accumulator
>--       accum = (take numMultis emptyMaps)
>--       maps = (map extractMap inMultis)
>--       condFilter' = accum maps cond (getKeys (head) 
>--      where
>--       numMultis = length inMultis
>--       emptyMaps = repeat Map.empty
>--       extractMap :: MultiValue -> Map ID SingleValue
>--       extractMap (MultiValue _ _ valMap) = valMap
>-- condFilter' :: [MultiValue] -> [MultiValue] -> Conditional -> [ID] -> [MultiValue]
>-- condFilter' accs inMultis cond keysLeft = 
>--       --extract name,col# pairs from multis
>--       --resolve cond into resolvedCond using names
>--       heads = map mvHead 
>--       map filter (eval cond ( map 
>--       let (id, values) = multiHead inMultis in
>--       case eval row of
>--         True ->
>--           condFil
>--      where
>        
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
> boxAsMulti x = MultiValue "_" "_" $ Map.singleton 0 x
>-- boxAsMulti (DibsInt64 i)  = MultiInt64  "_" "_" $ Map.insert 0 i Map.empty
>-- boxAsMulti (DibsChar c)   = MultiChar   "_" "_" $ Map.insert 0 c Map.empty
>-- boxAsMulti (DibsBool b)   = MultiBool   "_" "_" $ Map.insert 0 b Map.empty
>-- boxAsMulti (DibsString s) = MultiString "_" "_" $ Map.insert 0 s Map.empty
