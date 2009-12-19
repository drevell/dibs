This module defines operations on the table and column metadata, and defines
the datatypes for table and column storage.

> module Schema where
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Concurrent.STM
> import Control.Exception
> import Data.Int (Int64)
> import Util

> type ID = Int64

> data Schema = Schema (TVar (Map String Table)) 
> data Table = Table { tNextID :: TVar ID, tValMap :: TVar (Map String Column) }
> data ColType = StringColType | Int64ColType | BoolColType | CharColType
>       deriving Eq


A type class for Haskell types that can be used as Dibs values. There is a wrap
function for each type that wraps the value in an DibsPrim type.

> class BoxableHSPrim a where
>       wrap :: a ->  SingleValue
> instance BoxableHSPrim Int64 where
>       wrap x = DibsInt64 x
> instance BoxableHSPrim Char where
>       wrap x = DibsChar x
>-- instance BoxableHSPrim String where
>--       wrap x = DibsString x
> instance BoxableHSPrim Bool where
>       wrap x = DibsBool x

A SingleValue is a primitive type of the dibs system, and represents a single
cell of a table or MultiValue.

> data SingleValue = DibsInt64 Int64 | DibsChar Char | DibsString String 
>                  | DibsBool Bool 
>       deriving (Show, Eq)

A multivalue represents a column of data, either in an actual table, or part
of an intermediate result in memory.

> data MultiValue = MultiValue { tableName :: !String, 
>                                colName :: !String,
>                                mValMap :: !(Map ID SingleValue) }
>-- data MultiValue = MultiString String String (Map ID String) 
>--                 | MultiInt64 String String (Map ID Int64)
>--                 | MultiBool String String (Map ID Bool) 
>--                 | MultiChar String String (Map ID Char)
>       deriving Show

A column is a TVar containing a multivalue, which is shared between threads.

> data Column = Column { colMultiVal :: TVar MultiValue} 

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
>       tableMap <- readTVar schemaTv
>       case Map.lookup tableName tableMap of
>         Just _ -> error ("Table exists: " ++ tableName)
>         Nothing -> do 
>                      newTableTv <- newTVar Map.empty
>                      firstID <- newTVar 0
>                      let newTable = Table firstID newTableTv
>                      let newSchema = Map.insert tableName newTable tableMap
>                      writeTVar schemaTv newSchema
>                      -- Now the table exists, add columns 1 by 1
>                      mapM (addColumn schema tableName) columns
>                      return ()

Adds a column to a table, throwing an exception if the table doesn't exist or
if the column name already exists.

> addColumn :: Schema -> String -> (String, ColType) -> STM ()
> addColumn schema tableName (name, coltype) = do
>       t <- getTableByName schema tableName
>       let Table _ tableTv = t
>       tableMap <- readTVar tableTv
>       case Map.lookup name tableMap of
>         Just _ -> error ("Column "++name++" already exists!")
>         Nothing -> do
>           columnTv <- newTVar (MultiValue tableName name Map.empty)
>           let newColumn = Column columnTv
>           let newTableMap = Map.insert name newColumn tableMap
>           writeTVar tableTv newTableMap

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

> loadSchema :: IO Schema
> loadSchema = atomically $ do 
>       tv <- newTVar Map.empty
>       return (Schema tv)

Given a list of column names in a certain table, return the columns.

> getColumnsByName :: Table -> [String] -> STM [Column]
> getColumnsByName (Table _ tableTv) names = do
>       tableMap <- readTVar tableTv
>       return $ map (lookupSingle tableMap) names -- TODO: parallelize?
>      where
>       lookupSingle :: (Map String Column) -> String -> Column
>       lookupSingle tableMap name =
>           case Map.lookup name tableMap of
>               Nothing -> error $ "No such column: " ++ name
>               Just c -> c

Unpack a column or list of columns to get its underlying MultiValue

> columnToMulti :: Column -> STM MultiValue
> columnToMulti (Column tv) = readTVar tv
> columnsToMultis :: [Column] -> STM [MultiValue]
> columnsToMultis columns = mapM columnToMulti columns
