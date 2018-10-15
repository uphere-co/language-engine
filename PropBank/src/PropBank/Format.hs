{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module PropBank.Format where

import           Control.Lens
import           Data.Foldable               (toList)
import qualified Data.List            as L   (lookup)
-- import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as T.IO
--
import           NLP.Type.PennTreebankII
import           PropBank.Match
import           PropBank.Type.Match
import           PropBank.Type.Prop


formatRoleSetID :: RoleSetID -> T.Text
formatRoleSetID r = r^._1 <> "." <> r^._2


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


showInstance :: (PennTree,Instance) -> IO ()
showInstance (tr,prop) = do
  T.IO.putStrLn "---------------"
  let roleset = prop^.inst_lemma_roleset_id
  T.IO.putStrLn (formatRoleSetID roleset)
  T.IO.putStrLn "---------------"
  mapM_ (showArgument tr) (prop^.inst_arguments)


showNomInstance :: (PennTree,NomInstance) -> IO ()
showNomInstance (tr,nom) = do
  T.IO.putStrLn "---------------"
  showNomSense (tr,nom)
  T.IO.putStrLn "---------------"
  mapM_ (showArgument tr) (nom^.nominst_arguments)


showNomSense :: (PennTree,NomInstance) -> IO ()
showNomSense (tr,nom) = do
  let itr = zip ([0..] :: [Int]) $ toList tr
  print itr
  print (nom^.nominst_predicate_id)
  case L.lookup (nom^.nominst_predicate_id) $ zip [0..] $ toList tr of
    Nothing -> putStrLn "non-sense?"
    Just (_,n) -> T.IO.putStrLn (n <> "." <> T.pack (show (nom^.nominst_sense_number)))

  
showArgument :: PennTree -> Argument -> IO ()
showArgument tr arg = do
  putStr (show (arg^.arg_label) <> ": ")
  let format (t,n) = "(" <> t <> ") " <>   (T.intercalate " " . map (^._2._2) . toList) n
      itr = mkIndexedTree tr
  mapM_ (\x -> T.IO.putStr (maybe "Nothing" format (findNode x itr)) >> T.IO.putStr ", ") (arg^.arg_terminals)
  T.IO.putStr "\n"


showSentenceProp :: (Int,(PennTree,[Instance])) -> IO ()
showSentenceProp (i,(tr,props)) = do
  T.IO.putStrLn "================================================="
  T.IO.putStr ("Sentence " <> T.pack (show i) <> ": ") 
  (T.IO.putStrLn . T.intercalate " " . map (^._2) . toList) tr
  mapM_ (showInstance . (tr,)) props


showSentenceNom :: (Int,(PennTree,[NomInstance])) -> IO ()
showSentenceNom (i,(tr,props)) = do
  T.IO.putStrLn "================================================="
  T.IO.putStr ("Sentence " <> T.pack (show i) <> ": ") 
  (T.IO.putStrLn . T.intercalate " " . map (^._2) . toList) tr
  mapM_ (showNomInstance . (tr,)) props

