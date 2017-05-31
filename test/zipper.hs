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

tzipper :: TreeZipper Text Text
tzipper = TZ (PL') [("abc",[PL "test"],[PL "blah"])] 

tree :: Tree Text (Text,Text)
tree = PN "VP" [PN "NP" [PL ("VB","a"), PL ("NN","b"), PL ("DD","c")], PL ("CC","d")]


main = do
  -- print tzipper
  TIO.putStrLn $ prettyPrint 0 tree
  -- print (mkRootZipper tree)

  print (bifoldMap id (\(x,y) -> x<>y) tree)

  print (mkZipper [] tree)

  putStrLn "-----------"
  bimapM_ print print (mkZipper [] tree)

next (xs,y,z:zs) = let w = (y:xs,z,zs) in Just (w,w)
next (xs,y,[]) = Nothing
 
main2 = do
  print $ unfoldr next ([],1,[2,3,4,5])
