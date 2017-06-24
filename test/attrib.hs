module Main where

import Data.Attribute

main = do
  let x = "abc" <&> (3 :: Int) <&> anil
  print (ahead x)
  print (ahead (atail x))

  -- this gives a compile-time error
  --  print (ahead (atail (atail x)))
  
