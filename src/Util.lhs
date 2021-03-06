
> module Util where

 
A simple infinite loop
  
> forever :: IO a -> IO ()
> forever f = f >> forever f

Apply a function to a list of arguments. Runtime error if list has wrong length.

> applyList1 :: (a -> b) -> [a] -> b 
> applyList1 f (x:[]) = f x
> applyList2 :: (a -> a -> b) -> [a] -> b
> applyList2 f (x:y:[]) = f x y
> applyList3 :: (a -> a -> a -> b) -> [a] -> b
> applyList3 f (x:y:z:[]) = f x y z
> applyList4 :: (a -> a -> a -> a -> b) -> [a] -> b
> applyList4 f (w:x:y:z:[]) = f w x y z
> applyList5 :: (a -> a -> a -> a -> a -> b) -> [a] -> b
> applyList5 f (v:w:x:y:z:[]) = f v w x y z
> applyList6 :: (a -> a -> a -> a -> a -> a -> b) -> [a] -> b
> applyList6 f (u:v:w:x:y:z:[]) = f u v w x y z

Take a 3-argument function and turn it into a function on 3-tuples.

> uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
> uncurry3 f (x, y, z) = f x y z

> listCurry :: ([a] -> b) -> a -> a -> b
> listCurry f x y = f [x,y]

> listUncurry :: (a -> a -> c) -> [a] -> c
> listUncurry f (x:y:[]) = f x y