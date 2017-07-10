{-# LANGUAGE OverloadedStrings #-}

module PropBank.Format where

import           Control.Lens
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T.IO
--
import           PropBank.Type.Match
import           PropBank.Type.Prop


printMatchedNode :: MatchedArgNode -> IO ()
printMatchedNode x = do
  T.IO.putStrLn $ T.pack (show (x^.mn_node._1)) <> ":"
  print (x^.mn_trees)


printMatchedArg :: MatchedArgument -> IO ()
printMatchedArg x = do
  putStrLn $ show (x^.ma_argument.arg_label)
  mapM_ printMatchedNode (x^.ma_nodes)


printMatchedInst :: MatchedInstance -> IO ()
printMatchedInst x = do
  T.IO.putStrLn (x^.mi_instance.inst_lemma_type)
  putStrLn "---"  
  mapM_ printMatchedArg (x^.mi_arguments)
  putStrLn "---"  

