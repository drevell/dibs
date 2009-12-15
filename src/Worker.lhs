
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

> data NewConnection = MkNewConnection Handle HostName PortNumber

This is the function passed to forkIO to be a new thread. Several of these
are spawned on program startup.

> worker :: BoundedTChan NewConnection -> Schema -> IO ()
> worker connQueue schema = forever $ do 
>     MkNewConnection handle hostname port <- takeBTCIO connQueue -- blocking
>     hPutStrLn handle "Dibs ready."
>     repl schema "" handle `catch` (\_ -> return ())  -- ignore IO exceptions
>     hClose handle
>     return ()

This is the Read-Eval-Print Loop (REPL) run by worker threads.

> repl :: Schema -> String -> Handle -> IO ()
> repl schema stringSoFar handle =
>       if "\n\n" `isInfixOf` stringSoFar
>         then do
>           (parsedStmt, remaining) <- parse stringSoFar
>           result <- run schema parsedStmt
>           hPutStrLn handle $ show result
>           repl remaining handle
>         else
>           repl (stringSoFar ++ hGetLine handle) handle


