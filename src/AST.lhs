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
> import Monad
> import Control.Monad.State
> import Config

>-- class DibsAST a where
>--     run :: Schema -> a -> STM DibsValue
 

> data DibsAST = 
>       -- create table: name, list of pairs (name,type)
>       DibsCreate DibsAST DibsAST
>       -- get table contents: name, list of columns 
>       | DibsGet DibsAST DibsAST
>       -- insert into table: name, list of columns, list of lists of values 
>       | DibsInsert DibsAST DibsAST DibsAST 
>       -- execute in order left, right
>       | DibsSeq DibsAST DibsAST
>       -- 
>       | DibsIdentifier String
>       | DibsMultiply DibsAST DibsAST
>       | DibsDivide DibsAST DibsAST
>       | DibsAdd DibsAST DibsAST
>       | DibsSubtract DibsAST DibsAST
>       | DibsLiteral SingleValue
>       | DibsList [DibsAST]
>       | DibsTuple [DibsAST]
>       deriving (Show)

> data Scope = Scope (Map String EvalResult)
>            | NullScope

> data EvalResult = SingleEvalResult SingleValue
>                 | MVEvalResult [MultiValue]
>                 | ListEvalResult [EvalResult]
>       deriving (Show)

The Txn type is a monad that us used to keep track of scoped variables during 
the execution of a transaction. The AST node implementations will access the
variables by name, using the getVar and putVar functions, which use the
underlying Data.Map for storage.

> type Txn = StateT Scope (ReaderT ConfigData (WriterT LogEntry))

> getVar :: String -> Txn EvalResult
> getVar name = do
>       scope@(Scope varMap) <- get -- use underlying state monad
>       case Map.lookup name varMap of
>           Just x -> return x
>           Nothing -> error ("Reference to nonexistent variable: " ++ name)    
> putVar :: String -> EvalResult -> Txn ()
> putVar name value = do
>       scope@(Scope varMap) <- get --use underlying State monad
>       case Map.lookup name varMap of
>           Just x -> error ("Double-assignment to variable: " ++ name)
>           Nothing -> do
>               put $ Scope (Map.insert name value varMap) --put to State monad
>               return ()

Takes a list of multivalues containing one multivalue. That multivalue should
contain only one element. That element is returned. This is useful for AST
nodes whose result is a single value, e.g. DibsLiteral or DibsIdentifier.

> unpackSingleton :: [MultiValue] -> SingleValue
> unpackSingleton (mv:[]) = snd . head . Map.toList . mValMap $ mv
> unpackSingleton x = error ("Expected singleton but got: " ++ show x)
> unpackSingletonM :: STM [MultiValue] -> STM SingleValue
> unpackSingletonM mv = unpackSingleton `liftM` mv
>-- unpackSingletonM x = do { v <- x ; return unpackSingleton v}

Takes a list of multivalues, where each multivalue should contain only a single
value, and returns a list containing each of those values. This is useful
because the result of evaluating a DibsList will give a list of singletons that
is suitable for passing to this function.

> unpackList :: [MultiValue] -> [SingleValue]
> unpackList (m:ms) = (unpackSingleton [m]) : (unpackList ms)
> unpackList [] = []
> unpackListM :: STM [MultiValue] -> STM [SingleValue]
> unpackListM mvs = unpackList `liftM` mvs

Turn a SingleValue into a string. It must actually be a DibsString or there will
be an error.

> svToString :: SingleValue -> String
> svToString (DibsString s) = s
> svToString _ = error ("Tried to convert non-string SingleValue to string!")

> erToString :: EvalResult -> String
> erToString (SingleEvalResult (DibsString s)) = s
> erToString x = error ("Invalid argument to erToString: " ++ show x)

> erToSingleVal :: EvalResult -> SingleValue
> erToSingleVal (SingleEvalResult v) = v
> erToSingleVal x = error ("Cannot reduce to single value: " ++ show x)

> erToListSV :: EvalResult -> [SingleValue]
> erToListSV (ListEvalResult l) = map erToSingleVal l
> erToListSV x = error ("Not a list eval result: " ++ show x)

> runForString :: Schema -> DibsAST -> STM String
> runForString schema a = do
>       v <- run schema a
>       let SingleEvalResult sv = v
>       let DibsString s = sv
>       return s

> runForList :: Schema -> DibsAST -> STM [EvalResult]
> runForList schema a = do
>       vs <- run schema a
>       let ListEvalResult l = vs
>       return l

Implementations of the behavior for each AST node.

> run :: Schema -> DibsAST -> STM EvalResult
> run schema (DibsSeq l r) = do
>       run schema l -- discard result of first expr (use side effects)
>       run schema r  -- return result of second expr
> run schema (DibsCreate tableNameAST columnNamesAST) = do
>       tableName <- runForString schema tableNameAST
>       erColumnList <- runForList schema columnNamesAST
>       let columnList = map erToString erColumnList
>       success <- createTable schema tableName columnList :: STM Bool
>       return $ SingleEvalResult . DibsBool $ success
> run schema (DibsGet tableNameAST columnsAST) = do
>       tableName <- runForString schema tableNameAST
>       colNameListER <- runForList schema columnsAST
>       let colNameList = map erToString colNameListER
>       table <- getTableByName schema tableName
>       columns <- getColumnsByName table colNameList
>       multis <- columnsToMultis columns
>       return $ MVEvalResult multis
> run schema (DibsInsert tableNameAST namesAST rowsAST) = do
>       tableName <- runForString schema tableNameAST
>       nameListER <- runForList schema namesAST
>       let nameList = map erToString nameListER
>       nestedRowsER <- run schema rowsAST
>       let rows = erNest2ListToSv nestedRowsER 
>               -- ^ convert nested ListEvalResult to [[SingleValue]]
>       table <- getTableByName schema tableName
>       columns <- getColumnsByName table nameList
>       oldMultis <- columnsToMultis columns
>       nextID <- readTVar $ tNextID table
>       let insertMultis = rowsToMulti rows (nextID)
>       let finalMultis = mvListUnion oldMultis insertMultis
>       mapM (\(c,mv) -> writeTVar (colMultiVal c) mv) $ zip columns finalMultis
>       writeTVar (tNextID table) $ nextID + (fromIntegral (length rows))
>       return $ MVEvalResult insertMultis -- return stored values for use in expression
>      where
>       erNest2ListToSv :: EvalResult -> [[SingleValue]]
>       erNest2ListToSv (ListEvalResult ers) = map erToListSV ers
>       erNest2ListToSv x = error ("Not a ListEvalResult: " ++ show x)
>--      where
>--       mapLook :: (Ord k) => Map k a -> k -> a  -- wrapper to deal with Monad
>--       mapLook m k = case Map.lookup k m of
>--               Just x -> x
>--               Nothing -> error "Unexpected missing key in DibsInsert"
>--       permute permutation l =  [l !! x | x <- permutation]
>--       namesToMultis (name:names) multis = 
>--         let tree = Map.nameToMulti name ++ namesToMultis names
> run schema (DibsIdentifier s) = return $ SingleEvalResult . DibsString $ s
> run schema (DibsList l) = liftM ListEvalResult $ mapM (run schema) l
> run schema (DibsTuple l) = liftM ListEvalResult $ mapM (run schema) l
> run schema (DibsLiteral sv) = return $ SingleEvalResult sv
> run schema x  = error ("Unimplemented \"run\" for: " ++ show x)

Compute unions of MultiValues and lists of MultiValues

> mvUnion :: MultiValue -> MultiValue -> MultiValue
> mvUnion mv1 mv2 = 
>       let MultiValue tableName colName _ = mv1 -- take titles from 1st arg
>       in MultiValue tableName colName $ Map.union (mValMap mv1) (mValMap mv2)
> mvListUnion :: [MultiValue] -> [MultiValue] -> [MultiValue]
> mvListUnion [] [] = []
> mvListUnion (h1:t1) (h2:t2) = mvUnion h1 h2 : mvListUnion t1 t2
> mvListUnion [] _  = error "First list was longer, in mvListUnion"
> mvListUnion _ []  = error "Second list was longer, in mvListUnion" 

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
