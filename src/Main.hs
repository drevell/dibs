-- This module handles initial setup, creation of the worker threads, and
-- socket listen & accept.

module Main where

import Network
import IO
import Worker
import BoundedTChan 
import Control.Concurrent
import Util
import Schema
import Logger
import Config


main :: IO ()
main = do 
    putStrLn "Running dibs dev v1"
    let port = PortNumber 2420
    connQueue <- newBTCIO 500  -- new BoundedTChan for incoming connections
    schema <- loadSchema
    logBTChan <- spawnLogger
    let logFun = logmsg logBTChan
    let config = ConfigData schema connQueue logFun Dbg
    spawnWorkers config 10
    withSocketsDo $ do 
        listenSocket <- listenOn port
        acceptLoop config listenSocket
        return ()


acceptLoop :: ConfigData -> Socket -> IO ()
acceptLoop config listenSocket =
    forever $ do 
        (handle, hostname, port) <- accept listenSocket
        putBTCIO connQueue (MkNewConnection handle hostname port)
        return ()
   where
    connQueue = getBTChan config

spawnWorkers :: ConfigData -> Int -> IO ()
spawnWorkers config numWorkers = do 
    case numWorkers of 
        0 -> return ()
        _ -> do 
               forkIO $ worker config
               spawnWorkers config (numWorkers-1)
               return ()
             
spawnLogger :: IO (BoundedTChan (LogLevel, String))
spawnLogger = do
        btchan <- newBTCIO 500
        forkIO (logger btchan)
        return btchan
        
