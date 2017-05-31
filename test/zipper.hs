{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text           (Text)
import qualified Data.Text.IO as TIO
--
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper

tzipper :: TreeZipper Text Text
tzipper = TZ (PL') [("abc",[PL "test"],[PL "blah"])] 

tree :: Tree Text (Text,Text)
tree = PN "VP" [PN "NP" [PL ("VB","a"), PL ("NN","b"), PL ("DD","c")], PL ("CC","d")]


main = do
  -- print tzipper
  TIO.putStrLn $ prettyPrint 0 tree
  print (mkRootZipper tree)
