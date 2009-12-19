The reason that logging takes place in a
specific thread is because we don't want to slow down the worker threads with
IO. Ideally they will just quickly copy their message into the buffer of
messages to be logged and resume their work.

> module Logger where
> import Config
> import Util
> import BoundedTChan
> import Control.Concurrent.STM

The logmsg function is called by various other threads to log a message.

> logmsg :: BoundedTChan (LogLevel, String) -> ConfigData -> LogLevel -> String -> IO ()
> logmsg btchan config msgLevel msg = do
>       let minLevel = getLogLvl config
>       if msgLevel >= minLevel
>         then do
>           atomically $ putBTC btchan (msgLevel, msg) -- could fail with false
>           return ()
>         else
>           return ()  -- insufficient log level to log this message

This is the main logger thread. 

> logger :: BoundedTChan (LogLevel, String) -> IO ()
> logger btchan = forever $ do
>       (level, msg) <- takeBTCIO btchan  -- TODO: read in batches
>       putStrLn $ (show level) ++ ": " ++ msg
>       return ()

