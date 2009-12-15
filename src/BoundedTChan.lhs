This module defines a BoundedTChan data type, which is a TChan along with an 
enforced maximum size.

> module BoundedTChan (BoundedTChan, newBTC, newBTCIO, putBTC, putBTCIO, 
>                      takeBTC, takeBTCIO) where

> import Control.Concurrent.STM
> import Control.Concurrent.STM.TChan
> import Util

> data BoundedTChan a = MkBoundedTChan {btcGetChan :: TChan a, 
>                                       btcGetCount :: TVar Int,
>                                       btcGetSize :: TVar Int}

Adds an item to the boundedTChan if not full. Returns false if it's full, 
otherwise true.

> putBTC :: BoundedTChan a -> a -> STM Bool
> putBTC btchan x = do 
>       let MkBoundedTChan tc tvCount tvSize = btchan
>       beforeCount <- readTVar tvCount
>       size <- readTVar tvSize
>       if beforeCount < size                
>           then do
>               writeTChan tc x
>               writeTVar tvCount (beforeCount+1)
>               return True
>           else 
>               return False
              

A wrapper for callers who want to use putBTC in the IO monad, instead of STM.

> putBTCIO :: BoundedTChan a -> a -> IO Bool
> putBTCIO btchan x = 
>       atomically $ putBTC btchan x

Make a new, empty BoundedTChan

> newBTC :: Int -> STM (BoundedTChan a)
> newBTC size = do
>       chan <- newTChan
>       tvSize <- newTVar size
>       tvCount <- newTVar 0
>       return (MkBoundedTChan chan tvCount tvSize)

A convenient wrapper for callers who want to create a BoundedTChan in the IO
monad instead of the STM monad.

> newBTCIO :: Int -> IO (BoundedTChan a)
> newBTCIO size =
>       atomically $ newBTC size

Remove and return the next element from a BoundedTChan. Blocks if empty.

> takeBTC :: BoundedTChan a -> STM a
> takeBTC btchan = do
>       let MkBoundedTChan tchan tvCount tvSize = btchan
>       v <- readTChan tchan
>       oldCount <- readTVar tvCount
>       writeTVar tvCount (oldCount+1)
>       return v

A convenient wrapper around takeBTC that lives in the IO monad instead of STM

> takeBTCIO :: BoundedTChan a -> IO a
> takeBTCIO btchan = 
>       atomically $ takeBTC btchan