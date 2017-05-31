{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Control.Monad.State
import           Data.Bifoldable
import           Data.List           (unfoldr)
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text.IO as TIO
--
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper

{- 
tzipper :: TreeZipper Text Text
tzipper = TZ (PL') [("abc",[PL "test"],[PL "blah"])] 
-}

tree :: Tree Text (Text,Text)
tree = PN "VP" [PN "NP" [PL ("VB","a"), PL ("NN","b"), PL ("DD","c")], PL ("CC","d")]


main = do
  TIO.putStrLn $ prettyPrint 0 tree
  print (bifoldMap id (\(x,y) -> x<>y) tree)
  putStrLn "-----------"
  bimapM_ print print (mkTreeZipper [] tree)

 
main2 = do
  print $ mkListZipper [1,2,3]
