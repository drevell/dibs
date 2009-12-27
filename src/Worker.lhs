
The following compiler directive is necessary to allow the 

> {-# LANGUAGE ScopedTypeVariables #-}

> module Worker (worker, NewConnection(MkNewConnection)) where

> import Network
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Concurrent.STM.TChan
> import IO
> import BoundedTChan
> import Util
> import Data.List
> import Parser
> import Schema
> import AST
> import Logger
> import Char
> import ValueTypes
> import Txn
> import qualified Control.Exception as Exc

This is the function passed to forkIO to be a new thread. Several of these
are spawned on program startup.

> worker :: ConfigData -> IO ()
> worker config =  forever $ do
>     let connQueue = getConnBTChan config
>     putStrLn "Worker starting to wait for connection..."
>     MkNewConnection sock hostname port <- takeBTCIO connQueue -- blocking
>     putStrLn "Worker done blocking, has connection" 
>     (handleConnection config sock hostname port) `Exc.catches` 
>       (handlers sock hostname port) 
>     logmsg config Dbg ("Connection closed, thread looping again")
>    where
>--     handlers s h p  = 
>--       [ Exc.Handler (\(e :: Exc.ErrorCall)   -> handleErrorCall s h p e)
>--       , Exc.Handler (\(e :: Exc.IOException) -> handleIOException s h p e)]
>     handlers s h p  = 
>       [ Exc.Handler $ handleErrorCall s h p
>       , Exc.Handler $ handleIOException s h p]
>     handleErrorCall sock host port (e :: Exc.ErrorCall) = do
>       hPutStrLn sock ("Txn failed (log follows):\n" ++ show e)
>       logmsg config Warn (show e)
>       hClose sock
>     handleIOException sock host port (e :: Exc.IOException) = do
>       logmsg config Dbg ("IO exception on " ++ (show host) ++ ":" 
>               ++ (show port) ++ ": " ++ (show e))  
>       hClose sock

Called after we get a new connection from a client. When the client closes the
connection, there will be an exception, and this function will "return".

> handleConnection :: ConfigData -> Handle -> HostName -> PortNumber -> IO ()
> handleConnection cnf handle hostname port = do
>     hPutStrLn handle "Dibs ready."
>     logmsg cnf Dbg $ "New connection from " ++ hostname ++ ":" ++ (show port)
>     hSetBuffering handle LineBuffering  -- switch from block to line buffering
>     repl cnf handle ""
>     return ()

This is the Read-Eval-Print Loop (REPL) run by worker threads.

> repl :: ConfigData -> Handle -> String -> IO ()
> repl config handle stringSoFar = 
>       -- transactions are separated by "\n\n" or "\r\n\r\n"
>       if ("\n\n" `isInfixOf` stringSoFar) 
>       || ("\r\n\r\n" `isInfixOf` stringSoFar) 
>         then do
>           hPutStrLn handle "Parsing transaction..."
>           case dqlParse stringSoFar of
>             Left err  -> do
>               hPutStrLn handle ("Parse error: " ++ err)
>               logmsg config Info ("Client input parse error")
>               repl config handle ""
>             Right ast -> do
>               hPutStrLn handle "Running transaction..."
>               (result, logs) <- atomically $ runTxn config [] $ buildTxn ast
>               hPutStrLn handle $ "\nResult:\n=============="
>               hPutStrLn handle $ render result
>               hPutStrLn handle $ "=============="
>               mapM (logFromEntry config) logs -- send txn log entries to logger
>               repl config handle ""
>         else do 
>           let asInts = map ord stringSoFar :: [Int]
>           nextLine <- hGetLine handle
>           repl config handle $! (stringSoFar ++ nextLine ++ "\n")
>      where
>       schema = getConfSchema config

