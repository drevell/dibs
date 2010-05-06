> module TxnWriter where

> import Network
> import IO
> import BoundedTChan
> import ValueTypes
> import Control.Parallel
> import Control.Concurrent.STM
> import Control.Concurrent
> import AST
> import Util

The TxnWriter is a process that writes transactions to persistent storage as
they are committed, allowing the database to be restored from disk after a
shutdown or crash.

> startWriter :: ConfigData -> BoundedTChan (Maybe (TVar Bool), DibsAST) -> IO ()
> startWriter config chan = do
>   let persistLoc = persistLoc config
>   case persistLoc of
>       PersistFile filename -> do
>           handle <- openFile filename AppendMode -- TODO: handle errors
>           forkIO $ mainLoop config handle chan
>       x -> error $ "Unimplemented persistence method: " ++ show x

> mainLoop :: ConfigData -> Handle -> BoundedTChan (Maybe (TVar Bool)) -> IO ()
> mainLoop config handle chan = forever $ do
>   (notify, ast) <- takeBTC chan
>   hPutStr $ txnLogFormat txn
>   case notify of -- is the worker waiting to be informed of completion?
>       Just tv -> atomically $ writeTVar tv True
>       Nothing -> return ()
>   

> writeTxnLog :: DibsAST -> ConfigData -> IO ()
> writeTxnLog txn config = do
>   let btchan = getPersistBTChan config
>   case getDurability config of
>       WriteThrough -> do
>           tv <- atomically $ do
>               tv <- newTVar False
>               putBTC btchan (Just tv, txn)
>               return tv
>           atomically $ do
>               val <- readTVar tv
>               case val of
>                   True -> return ()
>                   False -> retry
>       WriteBack -> putBTCIO btchan (Nothing, txn)

>           
>            