This module defines operations on the table and column metadata.

> module Schema where
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Concurrent.STM
> import Control.Exception
> import Data.Int (Int64)
> import Util

> type ID = Int64

> data Schema = Schema (TVar (Map String Table)) 
> data Table = Table (TVar (Map String Column))
> data Column = StringCol (TVar (Map Int64 String)) 
>             | Int64Col (TVar (Map Int64 Int64))
>             | BoolCol (TVar (Map Int64 Bool))
>             | CharCol (TVar (Map Int64 Char))
> data ColType = StringColType | Int64ColType | BoolColType | CharColType
>   deriving Eq

Returns the Table corresponding to a given name, throwing an exception if it
doesn't exist.

> getTableByName :: Schema -> String -> STM Table
> getTableByName schema tableName = do
>       let Schema tablesTv = schema
>       tables <- readTVar tablesTv
>       case Map.lookup tableName tables of
>         Just t -> return t
>         Nothing -> error ("No such table: " ++ tableName)

Create a new table. Raises an exception if it already exists.

> createTable :: Schema -> String -> [(String, ColType)] -> STM ()
> createTable schema tableName columns = do
>       let Schema schemaTv = schema
>       tables <- readTVar schemaTv
>       case Map.lookup tableName tables of
>         Just _ -> error ("Table "++tableName++" exists!")
>         Nothing -> do 
>                      newTableTv <- newTVar Map.empty
>                      let newTable = Table newTableTv
>                      let newSchema = Map.insert tableName newTable tables
>                      writeTVar schemaTv newSchema
>                      -- Now the table exists, add columns 1 by 1
>                      mapM (addColumn schema tableName) columns
>                      return ()

Adds a column to a table, throwing an exception if the table doesn't exist or
if the column name already exists.

> addColumn :: Schema -> String -> (String, ColType) -> STM ()
> addColumn schema tableName (name, coltype) = do
>       t <- getTableByName schema tableName
>       let Table tableTv = t
>       tableMap <- readTVar tableTv
>       case Map.lookup name tableMap of
>         Just _ -> 
>           error ("Column "++name++" already exists!")
>         Nothing -> do
>           newColumn <- newColumnForType coltype
>           let newTableMap = Map.insert tableName newColumn tableMap
>           writeTVar tableTv newTableMap
>      where

Create a Column of a certain type, using the boxed type ColType.

> newColumnForType :: ColType -> STM Column
> newColumnForType colType
>       | colType == StringColType  = do 
>               tv <- newTVar Map.empty 
>               return (StringCol tv)
>       | colType == Int64ColType  = do 
>               tv <- newTVar Map.empty 
>               return (Int64Col tv)
>       | colType == BoolColType  = do 
>               tv <- newTVar Map.empty
>               return (BoolCol tv)
>       | colType == CharColType  = do 
>               tv <- newTVar Map.empty
>               return (CharCol tv)

Load or create the schema at startup.

> loadSchema :: IO Schema
> loadSchema = atomically $ do 
>       tv <- newTVar Map.empty
>       return (Schema tv)
