{-# LANGUAGE DataKinds #-}

module Main where

import Data.Attribute


main = do
  let x = "abc" <&> (3 :: Int) <&> anil
  print (ahead x)
  print (ahead (atail x))

  -- this gives a compile-time error
  --  print (ahead (atail (atail x)))

  print (There (Here :: Elem Int '[Int]) :: Elem Int '[String,Int])

  let y = getElem (There Here) x
  print y
  let y' = getElem Here x
  print y'
