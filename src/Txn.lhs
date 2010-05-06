
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> module Txn where

> import Control.Monad.State
> import Control.Monad.Reader
> import Control.Monad.Writer
> import Control.Monad.Trans
> import Control.Concurrent.STM
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Data.Int (Int64)
> import ValueTypes
> import Logger

The Txn type is a monad that is used during transaction execution to:
  - keep track of scoped variables during transaction execution using StateT
  - give read-only access to configuration data using ReaderT
  - give event logging using another StateT
  - give read & write access to TVars using STM. 

The following directive is needed to support the automatic deriving of the
several monadic type classes used in the "newtype Txn" declaration. I don't
fully understand how/why this works, but it is needed in order to build.

> newtype Txn a = Txn {
>   runTxn' :: StateT [Scope] (ReaderT ConfigData (StateT [LogEntry] (STM))) a
> } deriving (Monad, MonadReader ConfigData, MonadState [Scope]) 

> data Scope = Scope (Map String EvalResult) 

Access user-declared scoped variables during the transaction using getVar and
putVar. 

> getVar :: String -> Txn EvalResult
> getVar name = do
>       scopes <- get -- use underlying state monad
>       case checkAllScopes scopes name of
>           Just x -> return x
>           Nothing -> error ("Reference to nonexistent variable: " ++ name) 
>      where
>       checkAllScopes :: [Scope] -> String -> Maybe EvalResult
>       checkAllScopes [] _ = Nothing 
>       checkAllScopes ((Scope varMap):ss) name = 
>           case Map.lookup name varMap of
>               Just x -> Just x
>               Nothing -> checkAllScopes ss name -- look in parent scope
     
 putVar :: String -> EvalResult -> Txn ()
 putVar name value = do
       scopes <- get --use underlying State monad
       case Map.lookup name varMap of
           Just x -> error ("Double-assignment to variable: " ++ name)
           Nothing -> do
               put $ Scope (Map.insert name value varMap) --put to State monad
               return ()

> readTvTxn :: TVar a -> Txn a
> readTvTxn tvar = Txn . lift . lift . lift . readTVar $ tvar

> writeTvTxn :: TVar a -> a -> Txn ()
> writeTvTxn tv val = Txn . lift . lift . lift $ writeTVar tv val

> newTvTxn :: a -> Txn (TVar a)
> newTvTxn initial = Txn . lift . lift . lift $ newTVar initial

Given the values for config data, a variable scope, and a dibs transaction, run
the transaction. Will call error() if the transaction aborts.

> runTxn :: ConfigData -> [Scope] -> Txn a -> STM (a, [LogEntry])
> runTxn conf scopes t = 
>       let statePair = runStateT (runTxn' t) scopes
>           reader = fst `liftM` statePair -- statePair snd elem is [Scope]
>           logger = runReaderT reader conf
>           loggerPair = runStateT logger []
>       in do
>           (val, logs) <- loggerPair
>           return (val, reverse logs)

> getConf :: Txn ConfigData
> getConf = ask

> getSchema :: Txn Schema
> getSchema = liftM confSchema ask

Log a message by prepending it to the list of LogEntrys.

> tlog :: LogLevel -> String -> Txn ()
> tlog lvl msg = do
>       oldState <- Txn . lift . lift $ get
>       let newState = (LogEntry lvl msg) : oldState
>       Txn . lift . lift . put $ newState
  
txerror can be called to abort a dibs transaction (from an AST node, for 
example). It will take the existing logger state, convert all the log messages
into a single string, and return the string as the payload of an error().

> txerror :: String -> Txn a
> txerror msg = do
>       tlog Err msg
>       logmsgs <- Txn . lift . lift $ get 
>       error $ unlines . reverse . map show $ logmsgs

Add a new set of variable bindings to the scope.

> pushScope :: Scope -> Txn ()
> pushScope scope = do
>       oldScope <- get
>       put $ scope:oldScope

Remove the most recent / innermost set of bindings from the scope.

> popScope :: Txn ()
> popScope = do
>       (s:scopes) <- get
>       put scopes