{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad       ((<=<))
import           Data.Bifoldable
import           Data.List           (unfoldr)
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text.IO as TIO
--
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper


tree :: Tree Text (Text,Text)
tree = PN "VP" [PN "NP" [PL ("VB","a"), PL ("NN","b"), PL ("DD","c")], PL ("CC","d")]


main = do
  TIO.putStrLn $ prettyPrint 0 tree
  print (bifoldMap id (\(x,y) -> x<>y) tree)
  putStrLn "-----------"
  let z = mkTreeZipper [] tree
  bimapM_ print print z
  putStrLn "==========="
  let PN r [PN z' [PL z'',_,_]  , _ ] = z
  print (current z')
  print (current <$> (next z'))
  putStrLn "----------"
  print (current z'')
  print (current <$> (next z''))
  print (current <$> ((next <=< next) z''))
  print (current z'')
  putStrLn "-----------"
  print (current <$> (parent z''))
  print (current <$> ((parent <=< parent) z''))
  print (current <$> ((parent <=< parent <=< parent) z''))
  putStrLn "-----------"
  print (current <$> (child1 r))
  print (current <$> ((child1 <=< child1) r))
  print (current <$> ((child1 <=< child1 <=< child1) r))
  putStrLn "-----------"
  print $ match_test1 z
  print $ match_test2 z

match_test1 :: Tree (TreeZipper c t) (TreeZipper c t) -> Maybe (Tree c t)
match_test1 z = do
  PN _ [PN z' [_,PL z'',_]  , _ ] <- return z
  return (current z')


match_test2 :: Tree (TreeZipper c t) (TreeZipper c t) -> Maybe (Tree c t)
match_test2 z = do
  PN _ [PN z' [_,PL z'',_]  , _ , _] <- return z
  return (current z')

