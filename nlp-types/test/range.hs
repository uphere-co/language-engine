module Main where

import Data.Range


main :: IO ()
main = do
  let rngs = [(2,3),(5,7),(1,7),(2,7),(5,6)]
      rngs' = [(2,3),(5,7),(5,6)]

  print (rootRange rngs)
  print (partitionRanges rngs)
  print (partitionRanges rngs')
  print (rangeTree rngs)
  -- print (rangeTree rngs')

