The reason that logging takes place in a
specific thread is because we don't want to slow down the worker threads with
IO. Ideally they will just quickly copy their message into the buffer of
messages to be logged and go back to work.

> module Logger where
> import Util
> import BoundedTChan
> import ValueTypes
> import Control.Concurrent.STM
> import ValueTypes
> import System.IO.Unsafe

The logmsg function is called by other threads to log a message.

> logmsg :: ConfigData -> LogLevel -> String -> IO ()
> logmsg config msgLevel msg = msgLevel `seq` msg `seq` do
>       let minLevel = getConfLogLvl config
>       let btchan = getLogBTChan config
>       if msgLevel >= minLevel
>         then do
>           atomically $ putBTC btchan msg -- could fail with false
>           return ()
>         else
>           return ()  -- insufficient log level to log this message

This is the main logger thread. 

> logger :: BoundedTChan String -> IO ()
> logger btchan = forever $ do
>       msg <- takeBTCIO btchan  -- TODO: read in batches
>       putStrLn $ msg -- TODO: write to file

This is a wrapper that unpacks a LogEntry and invokes the logmsg function.

> logFromEntry :: ConfigData -> LogEntry -> IO ()
> logFromEntry config (LogEntry level msg) = logmsg config level msg

> unsafeLog :: String -> ()
> unsafeLog = unsafePerformIO . putStrLn