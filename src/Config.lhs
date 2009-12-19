
> module Config where
> import Schema
> import BoundedTChan
> import IO
> import Network
> import Control.Concurrent.STM

> data LogLevel = Dbg | Info | Warn | Err | Crit  deriving (Show, Ord, Eq)
> data NewConnection = MkNewConnection Handle HostName PortNumber

> data ConfigData = ConfigData { 
>                  getSchema :: Schema,  -- DB schema
>                  getBTChan :: BoundedTChan NewConnection, -- connection queue
>                  getLogFun :: ConfigData -> LogLevel -> String -> IO (), -- log function
>                  getLogLvl :: LogLevel}  -- log level (threshold)
