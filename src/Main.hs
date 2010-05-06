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
import ValueTypes
import TxnWriter

txnLogQueueSize = 1000
connectionQueueSize = 500
portNumber = 2420

main :: IO ()
main = do 
    putStrLn "Running dibs dev v1"
    let port = PortNumber portNumber
    connQueue <- newBTCIO connectionQueueSize
    logChan <- spawnLogger
    txnWriterChan <- newBTCIO txnLogQueueSize
    let preliminaryConfig = ConfigData NoSchema connQueue logChan Dbg 
            (PersistFile "./txn.log") txnWriterChan WriteThrough
    -- preliminaryConfig cannot yet have a valid schema stored in it 
    schema <- loadSchemaIO preliminaryConfig
    let finalConfig = preliminaryConfig {confSchema = schema, 
            connBTChan = connQueue, logBTChan = logChan, confLogLvl = Dbg}
    startWriter finalConfig txnWriterChan
    spawnWorkers finalConfig 50
    withSocketsDo $ do 
        listenSocket <- listenOn port
        acceptLoop finalConfig listenSocket
        return ()

acceptLoop :: ConfigData -> Socket -> IO ()
acceptLoop config listenSocket =
    forever $ do 
        (handle, hostname, port) <- accept listenSocket
        putBTCIO connQueue (MkNewConnection handle hostname port)
        return ()
   where
    connQueue = connBTChan config

spawnWorkers :: ConfigData -> Int -> IO ()
spawnWorkers config numWorkers = do 
    case numWorkers of 
        0 -> return ()
        _ -> do 
               forkIO $ worker config
               spawnWorkers config (numWorkers-1)
               return ()
             
spawnLogger :: IO (BoundedTChan String)
spawnLogger = do
        btchan <- newBTCIO 5000
        forkIO (logger btchan)
        return btchan
        
