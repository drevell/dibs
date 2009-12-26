This module defines operations on the table and column metadata, and defines
the datatypes for table and column storage.

> module Schema where
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Concurrent.STM
> import Control.Exception
> import Control.Monad
> import Data.Int (Int64)
> import Util
> import ValueTypes
> import Txn


A type class for Haskell types that can be used as Dibs values. There is a wrap
function for each type that wraps the value in an DibsPrim type.

>-- class BoxableHSPrim a where
>--       wrap :: a ->  SingleValue
>-- instance BoxableHSPrim Int64 where
>--       wrap x = DibsInt64 x
>-- instance BoxableHSPrim Char where
>--       wrap x = DibsChar x
>-- instance BoxableHSPrim String where
>--       wrap x = DibsString x
>-- instance BoxableHSPrim Bool where
>--       wrap x = DibsBool x


Returns the Table corresponding to a given name, throwing an exception if it
doesn't exist.

> getTableByName :: String -> Txn Table
> getTableByName tableName = do
>       Schema tablesTv <- getSchema
>       tables <- readTvTxn tablesTv
>       case Map.lookup tableName tables of
>         Just t -> return t
>         Nothing -> error ("No such table: " ++ tableName)

Create a new table. Raises an exception if it already exists.

> createTable :: String -> [String] -> Txn Bool
> createTable tableName columns = do
>       Schema schemaTv <- getSchema
>       tableMap <- readTvTxn schemaTv
>       case Map.lookup tableName tableMap of
>         Just _ -> return False
>         Nothing -> do 
>                      newTableTv <- newTvTxn Map.empty
>                      firstID <- newTvTxn 0
>                      let newTable = Table firstID newTableTv
>                      let newSchema = Map.insert tableName newTable tableMap
>                      writeTvTxn schemaTv newSchema
>                      -- Now the table exists, add columns 1 by 1
>                      mapM (addColumn tableName) columns
>                      return True

Adds a column to a table, throwing an exception if the table doesn't exist or
if the column name already exists.

> addColumn :: String -> String -> Txn ()
> addColumn tableName name = do
>       t <- getTableByName tableName
>       let Table _ tableTv = t
>       tableMap <- readTvTxn tableTv
>       case Map.lookup name tableMap of
>         Just _ -> error ("Column "++name++" already exists!")
>         Nothing -> do
>           columnTv <- newTvTxn (MultiValue tableName name Map.empty)
>           let newColumn = Column columnTv
>           let newTableMap = Map.insert name newColumn tableMap
>           writeTvTxn tableTv newTableMap

               tv <- newTVar (MultiString tableName name Map.empty)
               return $ Column tv
       newColumnForType BoolColType = do
               tv <- newTVar (MultiBool tableName name Map.empty)
               return $ Column tv
       newColumnForType Int64ColType = do
               tv <- newTVar (MultiInt64 tableName name Map.empty)
               return $ Column tv
       newColumnForType CharColType = do
               tv <- newTVar (MultiChar tableName name Map.empty)
               return $ Column tv

Load or create the schema at startup.

> loadSchemaIO :: ConfigData -> IO Schema
> loadSchemaIO config = liftM fst $ atomically $ runTxn config [] loadSchema
> loadSchema :: Txn Schema
> loadSchema = do 
>       tv <- newTvTxn Map.empty
>       return (Schema tv)

Given a list of column names in a certain table, return the columns.

> getColumnsByName :: Table -> [String] -> Txn [Column]
> getColumnsByName (Table _ tableTv) names = do
>       tableMap <- readTvTxn tableTv
>       return $ map (lookupSingle tableMap) names -- TODO: parallelize?
>      where
>       lookupSingle :: (Map String Column) -> String -> Column
>       lookupSingle tableMap name =
>           case Map.lookup name tableMap of
>               Nothing -> error $ "No such column: " ++ name
>               Just c -> c

Unpack a column or list of columns to get its underlying MultiValue

> columnToMulti :: Column -> Txn MultiValue
> columnToMulti (Column tv) = readTvTxn tv
> columnsToMultis :: [Column] -> Txn [MultiValue]
> columnsToMultis columns = mapM columnToMulti columns
