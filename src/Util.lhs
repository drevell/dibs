
> module Util where
 
A simple infinite loop
  
> forever :: IO a -> IO ()
> forever f = do { f ; forever f }

