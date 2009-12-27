This module includes the data type definitions for types that are widely used
throughout the program. There are problems with circular imports if these types
are defined in the locations where they logically belong. They are factored out
here, so they can be imported by many modules without creating the possibility 
of circular imports.

> module ValueTypes where
> import Data.Int (Int64)
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Network
> import IO
> import Control.Concurrent.STM
> import BoundedTChan
> import Util
> import List

A SingleValue is a primitive type of the dibs system, and represents a single
cell of a table or MultiValue.

> data SingleValue = DibsInt64 Int64 | DibsChar Char | DibsString String 
>                  | DibsBool Bool 
>       deriving (Show, Eq)
> instance Render SingleValue where
>   render (DibsInt64 x)  = show x
>   render (DibsChar x)   = show x
>   render (DibsString x) = show x
>   render (DibsBool x)   = show x

A unique ID for each row in a table.

> type ID = Int64

A multivalue represents a column of data, either in an actual table, or part
of an intermediate result in memory.

> data MultiValue = MultiValue { mvTableName :: !String, 
>                                mvColName :: !String,
>                                mvValMap :: (Map ID SingleValue) }
>-- data MultiValue = MultiString String String (Map ID String) 
>--                 | MultiInt64 String String (Map ID Int64)
>--                 | MultiBool String String (Map ID Bool) 
>--                 | MultiChar String String (Map ID Char)
>       deriving Show
> mvTableDotCol :: MultiValue -> String
> mvTableDotCol mv = mvTableName mv ++ "." ++ mvColName mv
> mvValList :: MultiValue -> [SingleValue]
> mvValList (MultiValue _ _ valMap) = map snd $ Map.toList valMap
> mvList :: MultiValue -> [(ID, SingleValue)]
> mvList (MultiValue _ _ valMap) = Map.toList valMap

A data type that will be used by the WriterT monad transformer inside
transactions to keep track of loggable events.

> data LogEntry = LogEntry LogLevel String
> instance Show LogEntry where
>       show (LogEntry lvl msg) = (show lvl) ++ ": " ++ (show msg)  

Used to control the verbosity of logging. A level is attached to each message,
and only messages above a configurable threshold will be logged.

> data LogLevel = Dbg | Info | Warn | Err | Crit  deriving (Show, Ord, Eq)

Used to send pending connections to worker threads.

> data NewConnection = MkNewConnection Handle HostName PortNumber

Data types used for in-memory representation of table row/column storage. All
storage in these types is intended to be strict.

> data Schema = Schema !(TVar (Map String Table)) | NoSchema
> data Table = Table { tNextID :: !(TVar ID), 
>                      tValMap :: !(TVar (Map String Column)) }
> data ColType = StringColType | Int64ColType | BoolColType | CharColType
>       deriving (Eq, Show)
> data Column = Column { colMultiVal :: !(TVar MultiValue)} 

A data type to capture a global configuration for the program.

> data ConfigData = ConfigData 
>                 { getConfSchema :: Schema
>                 , getConnBTChan :: BoundedTChan NewConnection -- connection queue
>                 , getLogBTChan :: BoundedTChan String
>                 , getConfLogLvl :: LogLevel}  -- log level (threshold)

> data EvalResult = SingleEvalResult SingleValue
>                 | MVEvalResult [MultiValue]
>                 | ListEvalResult [EvalResult]
>   deriving Show
> instance Render EvalResult where
>   render (MVEvalResult mvs) =
>       let colLists = map mvValList $ mvs  
>           rowLists = transpose colLists 
>           header = (joinList ", " $ map getMVName mvs) ++ "\n--------------"
>       in
>       unlines $ header : (showRows rowLists)
>      where
>       getMVName mv = mvTableDotCol mv 
>       showRows :: [[SingleValue]] -> [String]
>       showRows (r:rs) = (renderJoin ", " r) : showRows rs
>       showRows [] = []
>   render (SingleEvalResult sv) = render sv
>   render (ListEvalResult svList) = 
>       "[" ++ (renderJoin ", " svList) ++ "]"

   render (MVEvalResult mvs) = -- for MVEvalResults, we want a nice table
       let colLists = map flattenMV $ mvs  
           rowLists = transpose colLists 
           header = (joinList ", " $ map getMVName mvs) ++ "\n--------------"
       in
       showRows rowLists [header]
      where
       getMVName mv = (tableName mv) ++ "." ++ (colName mv) 
       showRows :: [[SingleValue]] -> [String] -> String
       showRows [] acc = unlines . reverse $ acc
       showRows (r:rs) acc = showRows rs $ (renderJoin ", " r) : acc 

A type class that will allow us to print tables of values for human consumption.

> class (Show a) => Render a where
>   render :: a -> String

Join a list of lists into just a list, using "sep" as a separator between them.

> joinList :: [a] -> [[a]] -> [a]
> joinList sep = foldl1 (\acc s -> acc ++ sep ++ s)

Render a list of renderable (member of type class Render) values, then join
the resulting list of strings using the giving separator.

> renderJoin :: (Render a) => String -> [a] -> String
> renderJoin sep xs = joinList sep $ (map render xs)

