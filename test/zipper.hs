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
  let z = mkTreeZipper [] tree
  bimapM_ print print z
  putStrLn "==========="
  let PN _ [PN z' [_,PL z'',_]  , _ ] = z
  print (current z')
  print (nextSibling z')

  print (current z'')
  print (prevSibling z'')

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

{- 
  print (nextSibling z')

  print (current z'')
  print (prevSibling z'')
  -}

main2 = do
  print $ mkListZipper [1,2,3]
