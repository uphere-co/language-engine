{-# LANGUAGE DataKinds #-}

module Main where

import Data.Attribute


main1 = do
  let x = "abc" `acons` (3 :: Int) `acons` anil
  print (ahead x)
  print (ahead (atail x))

  -- this gives a compile-time error
  --  print (ahead (atail (atail x)))

  print (There (Here :: Elem Int '[Int]) :: Elem Int '[String,Int])

  let y = getElem (There Here) x
  print y
  let y' = getElem Here x
  print y'


main = do
  let xs :: [(Int,String)]
      xs = [(1,"a"),(2,"b"),(3,"c")]

      ys :: [(Int,(Int,Int))]
      ys = [(1,(2,3)),(3,(4,5))]

      zs :: [(Int,Char)]
      zs = [(1,':'),(2,'*')]
      

  let xs' = map (\(i,x) -> i `acons` (x `acons` anil)) xs

  print xs'

  print $ joinAttrib fst ys xs'

  let lst = joinAttrib fst zs (joinAttrib fst ys xs') 

  print lst

  print (map toTuple lst :: [(Int,Maybe (Int,Char), Maybe (Int,(Int,Int)), String)])
