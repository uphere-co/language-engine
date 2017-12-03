module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Type

process = print

main = do
  putStrLn "create performance testing set"
  bstr <- BL.readFile "testset.json"
  case eitherDecode' bstr :: Either String [Test] of
    Left err -> putStrLn err
    Right lst -> mapM_ process lst
