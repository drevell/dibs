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
> import Util
> import qualified Data.ByteString.Lazy as B
> import Data.ByteString.Lazy (ByteString)
> import qualified Data.ByteString.Lazy.Char8 as BC
> import Data.Binary
> import qualified Data.Char


 data DibsAST = 
       -- create table: name, list of pairs (name,type)
       DibsCreate DibsAST DibsAST
       -- get table contents: name, list of columns 
       | DibsGet DibsAST DibsAST
       -- get table rows with condition: name, list of columns, bool conndition
       | DibsCondGet DibsAST DibsAST DibsAST  
       -- insert into table: name, list of columns, list of lists of values 
       | DibsInsert DibsAST DibsAST DibsAST 
       -- execute in order left, right
       | DibsSeq DibsAST DibsAST
       | DibsLessThan DibsAST DibsAST
       | DibsGreaterThan DibsAST DibsAST
       -- 
       | DibsIdentifier String
       | DibsMultiply DibsAST DibsAST
       | DibsDivide DibsAST DibsAST
       | DibsAdd DibsAST DibsAST
       | DibsSubtract DibsAST DibsAST
       | DibsEq DibsAST DibsAST
       | DibsNotEq DibsAST DibsAST
       | DibsLiteral SingleValue
       | DibsList [DibsAST]
       | DibsTuple [DibsAST]
       deriving (Show)



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
> erToSingleVal (ListEvalResult [x]) = erToSingleVal x
> erToSingleVal (MVEvalResult [mv]) | Map.size (mvValMap mv) == 1 = 
>   head $ Map.elems (mvValMap mv)
> erToSingleVal x = error $ "Cannot represent as single value: \"" 
>   ++ show x ++ "\""

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

Many dibs operations sum, difference, equality, etc can be expressed as a
function over a list of values which are the results of subexpressions. This
function gives us a concise way to reuse this pattern. 

Example: a sum would be: genericDibsOp sum [leftAST,rightAST]

SingleValue is an instance of Num and Ord, so any functions on those classes
can be used.

> genericDibsOp :: ([SingleValue] -> SingleValue) -> [DibsAST] -> Txn EvalResult
> genericDibsOp f subexprs = do
>   erList <- mapM buildTxn subexprs
>   let svList = map erToSingleVal erList
>   return $ SingleEvalResult $ f svList

> filterMVsByColName :: [String] -> [MultiValue] -> [MultiValue]
> filterMVsByColName _ [] = []
> filterMVsByColName colNames (mv:mvs) = 
>   let mvName = mvColName mv
>   in
>   if mvName `elem` colNames
>       then mv:(filterMVsByColName colNames mvs)
>       else filterMVsByColName colNames mvs 

Implementations of the behavior for each AST node. Should be loosely ordered
by popularity.

> buildTxn :: DibsAST -> Txn EvalResult
> buildTxn (DibsLessThan args@[l,r]) = 
>   genericDibsOp (\xs -> DibsBool $ listUncurry (<) xs) args
> buildTxn (DibsGreaterThan args@[l,r]) = 
>   genericDibsOp (\xs -> DibsBool $ listUncurry (>) xs) args
> buildTxn (DibsLessThanOrEq args@[l,r]) = 
>   genericDibsOp (\xs -> DibsBool $ listUncurry (<=) xs) args
> buildTxn (DibsGreaterThanOrEq args@[l,r]) = 
>   genericDibsOp (\xs -> DibsBool $ listUncurry (>=) xs) args
> buildTxn (DibsEq [l,r]) = 
>   genericDibsOp (\xs -> DibsBool $ listUncurry (==) xs) [l,r]
> buildTxn (DibsNotEq [l,r]) = 
>   genericDibsOp (\xs -> DibsBool $ listUncurry (/=) xs) [l,r]
> buildTxn (DibsSeq [l,r]) = do
>   buildTxn l -- discard result of first expr (use side effects)
>   buildTxn r  -- return result of second expr
> buildTxn (DibsGet [tableNameAST,columnsAST]) = do
>   tableName <- runForString tableNameAST
>   table <- getTableByName tableName
>   columns <- case columnsAST of
>       DibsAllColumns ->
>           getAllColumns table :: Txn [Column]
>       _ -> do 
>           colNameListER <- runForList columnsAST
>           let colNameList = map erToString colNameListER
>           getColumnsByName table colNameList
>   multis <- columnsToMultis columns
>   return $ MVEvalResult multis
> buildTxn (DibsCondGet [tableNameAST,colsAST,boolExpr]) = do
>   -- TODO: use a more efficient algorithm (last stage might be O(n log n)?)
>   tlog Dbg "DibsCondGet starting"
>   MVEvalResult uncondMvs <- buildTxn $ DibsGet [tableNameAST,DibsAllColumns]
>   tlog Dbg $ "Got unconditional results, num mvs: " ++ show (length uncondMvs)
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
>   tlog Dbg $ "nameToColNumMap: " ++ show (Map.toList nameToColNumMap) 
>   tlog Dbg "Starting cond_get filtering"
>   condRows <- filterM (satisfies boolExpr nameToColNum nameToColNumMap) uncondRows
>   tlog Dbg $ "uncondRows: " ++ show uncondRows
>   tlog Dbg $ "condRows: " ++ show condRows
>   let condColumns = case condRows of
>           [] -> take (length uncondMvs) (repeat []) -- need right # of cols
>           _  -> transpose condRows
>   let condMaps = map Map.fromList $ condColumns
>   let condMvs = map (uncurry3 MultiValue) $ zip3 tableNames colNames condMaps
>   filteredCondMvs <- case colsAST of
>           DibsAllColumns -> return condMvs
>           _ -> do
>               colNameList <- runForList colsAST
>               let colNames = map erToString colNameList
>               return $ filterMVsByColName colNames condMvs
>   tlog Dbg "Encapsulating cond_get result in MVs and returning"
>   tlog Dbg $ "filteredCondMvs: " ++ show filteredCondMvs
>   return $ MVEvalResult filteredCondMvs
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
> buildTxn (DibsInsert [tableNameAST,namesAST,rowsAST]) = do
>   tableName <- runForString tableNameAST
>   nameListER <- runForList namesAST
>   let nameList = map erToString nameListER
>   nestedRowsER <- buildTxn rowsAST
>   let rows = erNest2ListToSv nestedRowsER
>           -- ^ convert nested ListEvalResult to [[SingleValue]]
>   tlog Dbg ("Inserting " ++ (show . length $ rows) 
>           ++ " rows into table " ++ tableName)
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
> buildTxn (DibsIdentifier s) = getVar s
> buildTxn (DibsList l) = liftM ListEvalResult $ mapM buildTxn l
> buildTxn (DibsTuple l) = liftM ListEvalResult $ mapM buildTxn l
> buildTxn (DibsLiteral sv) = return $ SingleEvalResult sv
> buildTxn (DibsAdd [leftAST,rightAST]) = genericDibsOp sum [leftAST, rightAST]
> buildTxn (DibsSubtract [leftAST,rightAST]) = 
>   genericDibsOp (\[l,r] -> l-r) [leftAST, rightAST]
> buildTxn (DibsMultiply [leftAST,rightAST]) = genericDibsOp product [leftAST, rightAST]
> buildTxn (DibsDivide [leftAST,rightAST]) = 
>   genericDibsOp (\[l,r] -> l / r) [leftAST, rightAST]
> buildTxn (DibsCreate [tableNameAST,columnNamesAST]) = do
>   tableName <- runForString tableNameAST
>   erColumnList <- runForList columnNamesAST
>   tlog Dbg ("Creating table " ++ tableName)
>   let columnList = map erToString erColumnList
>   success <- createTable tableName columnList
>   return $ SingleEvalResult . DibsBool $ success
> buildTxn x = txerror $ "Unimplemented \"buildTxn\" for: " ++ show x

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

Convert a transaction AST tree to an external binary format suitable for a
transaction log.

> toTxnLog :: DibsAST -> ByteString
> toTxnLog = encode 

> idForAST (DibsCreate args)          = (1, Just args)
> idForAST (DibsGet args)             = (2, Just args)
> idForAST (DibsCondGet args)         = (3, Just args) 
> idForAST (DibsInsert args)          = (4, Just args) 
> idForAST (DibsSeq args)             = (5, Just args)
> idForAST (DibsLessThan args)        = (6, Just args)
> idForAST (DibsGreaterThan args)     = (7, Just args)
> idForAST (DibsGreaterThanOrEq args) = (8, Just args)
> idForAST (DibsLessThanOrEq args)    = (9, Just args)
> idForAST (DibsMultiply args)        = (10, Just args)
> idForAST (DibsDivide args)          = (11, Just args)
> idForAST (DibsAdd args)             = (12, Just args)
> idForAST (DibsSubtract args)        = (13, Just args)
> idForAST (DibsEq args)              = (14, Just args)
> idForAST (DibsNotEq args)           = (15, Just args)
> idForAST (DibsList args)            = (16, Just args)
> idForAST (DibsTuple args)           = (17, Just args)
> idForAST (DibsIdentifier _)         = (18, Nothing)
> idForAST (DibsLiteral _)            = (19, Nothing)
> idForAST (DibsAllColumns)           = (20, Nothing)

> constructorForId 1                  = DibsCreate
> constructorForId 2                  = DibsGet
> constructorForId 3                  = DibsCondGet
> constructorForId 4                  = DibsInsert
> constructorForId 5                  = DibsSeq
> constructorForId 6                  = DibsLessThan
> constructorForId 7                  = DibsGreaterThan
> constructorForId 8                  = DibsGreaterThanOrEq
> constructorForId 9                  = DibsLessThanOrEq
> constructorForId 10                 = DibsMultiply
> constructorForId 11                 = DibsDivide
> constructorForId 12                 = DibsAdd
> constructorForId 13                 = DibsSubtract
> constructorForId 14                 = DibsEq
> constructorForId 15                 = DibsNotEq
> constructorForId 16                 = DibsList
> constructorForId 17                 = DibsTuple

> instance Binary DibsAST where
>   put ast@(DibsIdentifier s) = do 
>       let (id, _) = idForAST ast 
>       putWord8 id
>       put $ BC.pack s
>   put ast@(DibsLiteral sv) = do 
>       let (id, _) = idForAST ast
>       putWord8 id
>       put sv
>   put ast@DibsAllColumns = do
>       let (id, _) = idForAST ast
>       putWord8 id
>   -- All other constructors are standard, their arg is just [DibsAST]
>   put ast = do 
>       let (id, Just args) = idForAST ast
>       putWord8 id
>       put args
>   get = do
>           id <- getWord8
>           case id of
>               18 -> do -- DibsIdentifier String
>                   nameBS <- get
>                   let name = BC.unpack nameBS
>                   return $ DibsIdentifier name
>               19 -> do -- DibsLiteral SingleValue
>                   sv <- get :: Get SingleValue
>                   return $ DibsLiteral sv
>               20 -> do -- DibsAllColumns
>                   return DibsAllColumns
>               _ -> do
>                   args <- get :: Get [DibsAST]
>                   return $ (constructorForId id) args

ASTs have a compact binary representation that is good for quick parsing.

>-- instance BinConvert DibsAST where
>-- toBin ast@(DibsIdentifier s) = idForAST ast `parensWith` [pack s] 
>-- toBin ast@(DibsLiteral sv) = idForAST ast `parensWith` [svToBin sv]
>-- toBin ast@(DibsAllColumns) = idForAST ast `parensWith` [B.empty]  
> -- All other AST constructors have a standard [DibsAST] argument list
>-- toBin ast = let (id, args) = idForAST ast in
>--       genericBin id (map toBin args)

Most AST nodes require this operation for conversion to binary, so it's factored
out here.

>-- genericBin :: Word8 -> [DibsAST] -> ByteString
>-- genericBin n xs =
>--   n `parensWith` (map toBin xs)
 