module Main where

import System.Environment (getArgs)
import NER


main :: IO ()
main = do
  args <- getArgs
  saveCompanyInfo (args !! 0) (args !! 1)
