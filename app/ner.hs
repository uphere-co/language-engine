module Main where

import NER

main :: IO ()
main = do
  nt <- loadNameTable 
  parseCompany nt
