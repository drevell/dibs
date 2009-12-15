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

main :: IO ()
main = do 
    putStrLn "Running dibs dev v1"
    let port = PortNumber 2420
    connQueue <- newBTCIO 500  -- new BoundedTChan for incoming connections
    schema <- loadSchema
    spawnWorkers schema connQueue 10
    withSocketsDo $ do 
        listenSocket <- listenOn port
        acceptLoop listenSocket connQueue
        return ()


acceptLoop :: Socket -> BoundedTChan NewConnection -> IO ()
acceptLoop listenSocket connQueue =
    forever $ do 
        (handle, hostname, port) <- accept listenSocket
        putBTCIO connQueue (MkNewConnection handle hostname port)
        return ()

spawnWorkers :: BoundedTChan NewConnection -> Int -> IO ()
spawnWorkers connQueue numWorkers = do 
    case numWorkers of 
        0 -> return ()
        _ -> do 
               forkIO $ worker connQueue
               spawnWorkers connQueue (numWorkers-1)
               return ()
             

