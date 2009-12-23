
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
> import Config
> import Char

This is the function passed to forkIO to be a new thread. Several of these
are spawned on program startup.

> worker :: ConfigData -> IO ()
> worker config =  do
>     let connQueue = getBTChan config
>     MkNewConnection handle hostname port <- takeBTCIO connQueue -- blocking
>     let finallyF = hClose handle
>     finallyF `finallyAfter` (handleConnection config handle hostname port)
>     log Dbg ("Connection dropped, thread looping again")
>     worker config
>    where
>     f `finallyAfter` g = do {g `catch` (\_ -> return ()); f}
>     log = getLogFun config $ config

Called after we get a new connection from a client. When the client closes the
connection, there will be an exception, and this function will "return".

> handleConnection :: ConfigData -> Handle -> HostName -> PortNumber -> IO ()
> handleConnection config handle hostname port = do
>     hPutStrLn handle "Dibs ready."
>     log Dbg $ "New connection from " ++ hostname ++ ":" ++ (show port)
>     hSetBuffering handle LineBuffering  -- switch from block to line buffering
>     repl config handle ""
>     return ()
>    where
>     log = getLogFun config $ config

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
>               log Info ("Client input parse error")
>               repl config handle ""
>             Right parsed -> do
>               hPutStrLn handle "Running transaction..."
>               result <- atomically $ run schema parsed NullScope
>               hPutStrLn handle $ "Result: " ++ (show result)
>               repl config handle ""
>         else do 
>           let asInts = map ord stringSoFar :: [Int]
>           nextLine <- hGetLine handle
>           repl config handle $! (stringSoFar ++ nextLine ++ "\n")
>      where
>       schema = getSchema config
>       log = getLogFun config config

