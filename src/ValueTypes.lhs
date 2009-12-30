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
>                  | DibsBool Bool | DibsDouble Double
>       deriving (Show)
> instance Render SingleValue where
>   render (DibsInt64 x)  = show x
>   render (DibsDouble x) = show x
>   render (DibsChar x)   = show x
>   render (DibsString x) = show x
>   render (DibsBool x)   = show x
> instance Num SingleValue where
>   (+) (DibsInt64 x) (DibsInt64 y) = DibsInt64 $ x + y
>   (+) (DibsInt64 x) (DibsDouble y) = DibsDouble $ fromIntegral x + y
>   (+) x@(DibsDouble _) y@(DibsInt64 _) = y + x -- flip, use above definition
>   (+) (DibsDouble x) (DibsDouble y) = DibsDouble $ x + y
>   (+) x y = error $ "Invalid operands for +: " ++ show x ++ ", " ++ show y
>   (-) x y = x + negate y
>   (*) (DibsInt64 x) (DibsInt64 y) = DibsInt64 $ x * y
>   (*) (DibsInt64 x) (DibsDouble y) = DibsDouble $ fromIntegral x * y
>   (*) x@(DibsDouble _) y@(DibsInt64 _) = y * x
>   (*) (DibsDouble x) (DibsDouble y) = DibsDouble $ x * y
>   (*) x y = error $ "Invalid operands for *: " ++ show x ++ ", " ++ show y
>   negate (DibsInt64 x) = DibsInt64 ((-1) * x)
>   negate (DibsDouble x)  = DibsDouble ((-1.0) * x)
>   negate x = error $ "Invalid operand to negate: " ++ show x
>   abs (DibsInt64 x) = DibsInt64 $ abs x
>   abs (DibsDouble d) = DibsDouble $ abs d
>   abs x = error $ "Invalid operand to absolute value: " ++ show x
>   signum x | x < 0 = DibsInt64 (-1)
>   signum x | x > 0 = DibsInt64 1
>   signum x | x == 0 = DibsInt64 0
>   fromInteger x = DibsInt64 $ fromIntegral x

> instance Ord SingleValue where
>   compare x y | x == y = EQ
>   compare (DibsInt64 x) (DibsInt64 y) = compare x y
>   compare (DibsDouble x) (DibsDouble y) = compare x y
>   compare (DibsInt64 x) (DibsDouble y) = compare (fromIntegral x) y
>   compare l@(DibsDouble _) r@(DibsInt64 _) = compare r l -- reverse, use above def 
>   compare x y = error $ "Type error in comparison between: \"" ++ show x 
>       ++ "\" and \"" ++ show y ++ "\""

> instance Eq SingleValue where
>   (DibsInt64 x) == (DibsInt64 y) = x == y
>   (DibsInt64 x) == (DibsDouble y) = fromIntegral x == y
>   x@(DibsDouble _) == y@(DibsInt64 _) = y == x -- reverse and use previous def
>   (DibsDouble x) == (DibsDouble y) = x == y
>   (DibsBool x) == (DibsBool y) = x == y
>   (DibsString x) == (DibsString y) = x == y
>   (DibsChar x) == (DibsChar y) = x == y
>   x == y = error $ "Can't evaluate equality between " ++ show x ++ " and "
>       ++ show y

> instance Fractional SingleValue where
>   fromRational r = DibsDouble (fromRational r :: Double)
>   (DibsInt64 x) / (DibsInt64 y) = DibsInt64 $ x `div` y
>   (DibsInt64 x) / (DibsDouble y) = DibsDouble $ fromIntegral x / y
>   (DibsDouble x) / (DibsInt64 y) = DibsDouble $ x / fromIntegral y
>   (DibsDouble x) / (DibsDouble y) = DibsDouble $ x / y

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
>       joinList "\n" $ header : (showRows rowLists)
>      where
>       getMVName mv = mvTableDotCol mv 
>       showRows :: [[SingleValue]] -> [String]
>       showRows (r:rs) = (renderJoin ", " r) : showRows rs
>       showRows [] = []
>   render (SingleEvalResult sv) = render sv
>   render (ListEvalResult svList) = 
>       "[" ++ (renderJoin ", " svList) ++ "]"

A type class that will allow us to print tables of values for human consumption.

> class (Show a) => Render a where
>   render :: a -> String

Join a list of lists into just a list, using "sep" as a separator between them.

> joinList :: [a] -> [[a]] -> [a] -- TODO: lazier
> --joinList sep l@(_:_) = foldl1 (\acc s -> acc ++ sep ++ s) l
> joinList sep (x:(xs@(_:_))) = x ++ sep ++ joinList sep xs
> joinList sep (x:[]) = x -- recursive base case
> joinList sep [] = []  -- not base case, only used when called with empty list

Render a list of renderable (member of type class Render) values, then join
the resulting list of strings using the giving separator.

> renderJoin :: (Render a) => String -> [a] -> String
> renderJoin sep xs@(_:_) = joinList sep $ (map render xs)
> renderJoin _ [] = error "renderJoin called with empty list"
