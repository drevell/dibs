A bunch of types and typeclasses that represent an abstract syntax tree for
the Dibs transaction language.

There has to be some better way to express the "buildTxn" function, instead of as a
series of guards. This could probably use type classes in some elegant way 
that I don't yet understand.

> module AST where
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Schema
> import List
> import Monad
> import ValueTypes
> import Txn
> import Logger

> data DibsAST = 
>       -- create table: name, list of pairs (name,type)
>       DibsCreate DibsAST DibsAST
>       -- get table contents: name, list of columns 
>       | DibsGet DibsAST DibsAST
>       -- get table rows with condition: name, list of columns, bool conndition
>       | DibsCondGet DibsAST DibsAST DibsAST  
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

Takes a list of multivalues containing one multivalue. That multivalue should
contain only one element. That element is returned. This is useful for AST
nodes whose result is a single value, e.g. DibsLiteral or DibsIdentifier.

> unpackSingleton :: [MultiValue] -> SingleValue
> unpackSingleton (mv:[]) = snd . head . Map.toList . mvValMap $ mv
> unpackSingleton x = error ("Expected singleton but got: " ++ show x)
> unpackSingletonM :: Txn [MultiValue] -> Txn SingleValue
> unpackSingletonM mv = unpackSingleton `liftM` mv
>-- unpackSingletonM x = do { v <- x ; return unpackSingleton v}

Takes a list of multivalues, where each multivalue should contain only a single
value, and returns a list containing each of those values. This is useful
because the result of evaluating a DibsList will give a list of singletons that
is suitable for passing to this function.

> unpackList :: [MultiValue] -> [SingleValue]
> unpackList (m:ms) = (unpackSingleton [m]) : (unpackList ms)
> unpackList [] = []
> unpackListM :: (Monad m) => m [MultiValue] -> m [SingleValue]
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

> runForString :: DibsAST -> Txn String
> runForString a = do
>       v <- buildTxn a
>       let SingleEvalResult sv = v
>       let DibsString s = sv
>       return s

> runForList :: DibsAST -> Txn [EvalResult]
> runForList a = do
>       vs <- buildTxn a
>       let ListEvalResult l = vs
>       return l

Implementations of the behavior for each AST node.

> buildTxn :: DibsAST -> Txn EvalResult
> buildTxn (DibsSeq l r) = do
>   buildTxn l -- discard result of first expr (use side effects)
>   buildTxn r  -- return result of second expr
> buildTxn (DibsGet tableNameAST columnsAST) = do
>   tableName <- runForString tableNameAST
>   colNameListER <- runForList columnsAST
>   let colNameList = map erToString colNameListER
>   table <- getTableByName tableName
>   columns <- getColumnsByName table colNameList
>   multis <- columnsToMultis columns
>   return $ MVEvalResult multis
> buildTxn (DibsCondGet tableName rows boolExpr) = do
>   -- TODO: use a more efficient algorithm
>   MVEvalResult uncondMvs <- buildTxn $ DibsGet tableName rows
>   let tableNames = map mvTableName uncondMvs
>   let colNames = map mvColName uncondMvs
>   let tableDotColNames = map mvTableDotCol uncondMvs 
>   -- We create bindings under names "table.col" and also just "col"
>   let nameToColNumMap = Map.fromList $  
>         (zip tableDotColNames [0..]) ++ (zip colNames [0..])
>   let nameToColNum = \s -> case Map.lookup s nameToColNumMap of
>           Just n -> n
>           Nothing -> error $ "Cond get: nonexistent column: " ++ s 
>   let uncondColLists = map mvList uncondMvs 
>   let uncondRows = transpose uncondColLists
>   condRows <- filterM (satisfies boolExpr nameToColNum nameToColNumMap) uncondRows
>   let condColumns = map Map.fromList $ transpose condRows
>   let namedColTuples = zip3 tableNames colNames condColumns
>   let condMvs = map (\(tn, cn, col) -> MultiValue tn cn col) namedColTuples
>   return $ MVEvalResult condMvs
>  where
>   satisfies :: DibsAST -> (String -> Int) -> Map String Int -> [(ID, SingleValue)] -> Txn Bool
>   satisfies expr nameToCol nameToColMap vals = do 
>       -- Evalute with a Scope that binds the variables in this row
>       let rowSvMap = Map.map (\v -> snd $ vals !! v) nameToColMap
>       let rowErMap = Map.map (SingleEvalResult) rowSvMap
>       pushScope $ Scope rowErMap
>       result <- buildTxn expr
>       popScope
>       case result of 
>           SingleEvalResult (DibsBool b) -> return b
>           otherwise -> txerror $ "Conditional get expression not boolean!"
> buildTxn (DibsInsert tableNameAST namesAST rowsAST) = do
>   tableName <- runForString tableNameAST
>   nameListER <- runForList namesAST
>   let nameList = map erToString nameListER
>   nestedRowsER <- buildTxn rowsAST
>   let rows = erNest2ListToSv nestedRowsER
>           -- ^ convert nested ListEvalResult to [[SingleValue]]
>   tlog Dbg ("Inserting " ++ (show . length $ rows) 
>           ++ " into table " ++ tableName)
>   table <- getTableByName tableName
>   columns <- getColumnsByName table nameList
>   oldMultis <- columnsToMultis columns
>   nextID <- readTvTxn $ tNextID table
>   let insertMultis = rowsToMulti rows (nextID)
>   let combinedMultis = mvListUnion oldMultis insertMultis
>   mapM (\(c,mv) -> writeTvTxn (colMultiVal c) mv) $ zip columns combinedMultis
>   writeTvTxn (tNextID table) $ nextID + (fromIntegral (length rows))
>   return $ SingleEvalResult $ DibsBool True
>  where
>   erNest2ListToSv :: EvalResult -> [[SingleValue]]
>   erNest2ListToSv (ListEvalResult ers) = map erToListSV ers
>   erNest2ListToSv x = error ("Not a ListEvalResult: " ++ show x)
> buildTxn (DibsIdentifier s) = return $ SingleEvalResult . DibsString $ s
> buildTxn (DibsList l) = liftM ListEvalResult $ mapM buildTxn l
> buildTxn (DibsTuple l) = liftM ListEvalResult $ mapM buildTxn l
> buildTxn (DibsLiteral sv) = return $ SingleEvalResult sv
> buildTxn (DibsCreate tableNameAST columnNamesAST) = do
>   tableName <- runForString tableNameAST
>   erColumnList <- runForList columnNamesAST
>   tlog Dbg ("Creating table " ++ tableName)
>   let columnList = map erToString erColumnList
>   success <- createTable tableName columnList
>   return $ SingleEvalResult . DibsBool $ success
> buildTxn x = txerror ("Unimplemented \"buildTxn\" for: " ++ show x)

Compute unions of MultiValues and lists of MultiValues

> mvUnion :: MultiValue -> MultiValue -> MultiValue
> mvUnion mv1 mv2 = 
>       let MultiValue tableName colName _ = mv1 -- take titles from 1st arg
>       in MultiValue tableName colName $ 
>           Map.union (mvValMap mv1) (mvValMap mv2)
> mvListUnion :: [MultiValue] -> [MultiValue] -> [MultiValue]
> mvListUnion [] [] = []
> mvListUnion (h1:t1) (h2:t2) = mvUnion h1 h2 : mvListUnion t1 t2
> mvListUnion [] _  = error "First list was longer, in mvListUnion"
> mvListUnion _ []  = error "Second list was longer, in mvListUnion" 

> -- convert from a row-major list of rows to a column-major list of multivalues
> rowsToMulti :: [[SingleValue]] -> ID -> [MultiValue]
> rowsToMulti rowLists startID = 
>       let rowWidth = length $ head rowLists
>           colLists = transpose rowLists 
>       in
>       [MultiValue "_" "_" (Map.fromList (zip [startID..] colList)) 
>               | colList <- colLists]

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
