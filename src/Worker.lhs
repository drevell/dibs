
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

This is the function passed to forkIO to be a new thread. Several of these
are spawned on program startup.

> worker :: ConfigData -> IO ()
> worker config = forever $ do 
>     let connQueue = getBTChan config
>     MkNewConnection handle hostname port <- takeBTCIO connQueue -- blocking
>     (getLogFun config) config (getLogLvl config) "Dibs ready."
>     hSetBuffering handle LineBuffering  -- switch from block to line buffering
>     repl config handle "" `catch` (\_ -> return ())  -- ignore IO exceptions
>     hClose handle
>     return ()

This is the Read-Eval-Print Loop (REPL) run by worker threads.

> repl :: ConfigData -> Handle -> String -> IO ()
> repl config handle stringSoFar =
>       if "\n\n" `isInfixOf` stringSoFar  -- transactions separated by \n\n
>         then do
>           hPutStrLn handle "Parsing transaction..."
>           let (parsedStmt, remaining) = parse stringSoFar
>           hPutStrLn handle "Running transaction..."
>           result <- atomically $ run schema parsedStmt
>           hPutStrLn handle $ "Result: " ++ (show result)
>           repl config handle remaining
>         else do 
>           --log Dbg "No double-newline so far, looping"
>           --log Dbg ("Buffer is: " ++ stringSoFar) 
>           anotherLine <- hGetLine handle
>           repl config handle (stringSoFar ++ anotherLine ++ "\n")
>      where
>       schema = getSchema config
>       log = getLogFun config config

