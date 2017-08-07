{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens               hiding (levels)
import           Data.Foldable
import qualified Data.IntMap                as IM
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Clause
import           NLP.Type.PennTreebankII


testpt1,testpt2,testpt3,testpt4,testpt5 :: PennTree
testlemma1,testlemma2,testlemma3,testlemma4,testlemma5 :: [(Int,Text)]


testpt1 = PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","man")],PN "PRN" [PL (",",","),PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","seems")]],PL (",",",")],PN "VP" [PL ("VBZ","has"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","Lichtenstein"),PL ("NN","corporation")],PL (",",","),PN "VP" [PL ("VBN","licensed"),PN "PP" [PL ("IN","in"),PN "NP" [PN "NP" [PL ("NNP","Libya")],PL ("CC","and"),PN "NP" [PL ("JJ","sheltered")]]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","the"),PL ("NNPS","Bahamas")]]]]],PL (".",".")]]

testlemma1 = [(0,"the"),(1,"man"),(2,","),(3,"it"),(4,"seem"),(5,","),(6,"have"),(7,"a"),(8,"lichtenstein"),(9,"corporation"),(10,","),(11,"license"),(12,"in"),(13,"Libya"),(14,"and"),(15,"sheltered"),(16,"in"),(17,"the"),(18,"Bahamas"),(19,".")]


testpt2 = PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("VBZ","'s"),PN "VP" [PN "ADVP" [PL ("RB","actively")],PN "VP" [PL ("VBG","considering"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","breakup")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","giant"),PL ("NNP","Wall"),PL ("NNP","Street"),PL ("NNS","banks")]]]],PL (",",","),PN "VP" [PL ("VBG","giving"),PN "NP" [PL ("DT","a"),PL ("NN","push")],PN "PP" [PL ("TO","to"),PN "NP" [PL ("NNS","efforts")]],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","revive"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","Depression-era"),PL ("NN","law")],PN "VP" [PL ("VBG","separating"),PN "NP" [PL ("NN","consumer"),PL ("CC","and"),PL ("NN","investment"),PL ("NN","banking")]]]]]]]]]]]],PL (".",".")]]

testlemma2 = [(0,"President"),(1,"Donald"),(2,"Trump"),(3,"say"),(4,"he"),(5,"be"),(6,"actively"),(7,"consider"),(8,"a"),(9,"breakup"),(10,"of"),(11,"giant"),(12,"Wall"),(13,"Street"),(14,"bank"),(15,","),(16,"give"),(17,"a"),(18,"push"),(19,"to"),(20,"effort"),(21,"to"),(22,"revive"),(23,"a"),(24,"depression-era"),(25,"law"),(26,"separate"),(27,"consumer"),(28,"and"),(29,"investment"),(30,"banking"),(31,".")]


testpt3 = PN "ROOT" [PN "S" [PN "NP" [PL ("NNS","Carmakers"),PL (",",","),PL ("NN","rideshare"),PL ("NNS","services"),PL ("CC","and"),PL ("NN","tech"),PL ("NNS","companies")],PN "VP" [PL ("VBP","are"),PN "VP" [PL ("VBG","teaming"),PN "PRT" [PL ("RP","up")],PN "PP" [PL ("IN","in"),PN "NP" [PN "NP" [PL ("DT","an"),PN "ADJP" [PL ("RB","increasingly"),PL ("JJ","complex")],PL ("NN","series")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","alliances")]]]]]],PL (".",".")]]

testlemma3 = [(0,"carmaker"),(1,","),(2,"rideshare"),(3,"service"),(4,"and"),(5,"tech"),(6,"company"),(7,"be"),(8,"team"),(9,"up"),(10,"in"),(11,"a"),(12,"increasingly"),(13,"complex"),(14,"series"),(15,"of"),(16,"alliance"),(17,".")]


testpt4 = PN "ROOT" [PN "S" [PL ("CC","But"),PN "NP" [PL ("DT","that"),PL ("NN","tenet")],PN "VP" [PL ("VBD","was"),PN "VP" [PN "VP" [PL ("VBN","undone"),PN "PP" [PL ("IN","in"),PN "NP" [PL ("CD","1999")]]],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","move")],PN "SBAR" [PN "WHNP" [PL ("WDT","that")],PN "S" [PN "VP" [PL ("VBZ","'s"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","blamed"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("DT","some")],PN "PP" [PL ("IN","for"),PN "NP" [PL ("DT","the"),PL ("CD","2008"),PL ("NN","market"),PL ("NN","crash")]]]]]]]]]]]],PL (".",".")]]

testlemma4 = [(0,"but"),(1,"that"),(2,"tenet"),(3,"be"),(4,"undo"),(5,"in"),(6,"1999"),(7,","),(8,"a"),(9,"move"),(10,"that"),(11,"be"),(12,"be"),(13,"blame"),(14,"by"),(15,"some"),(16,"for"),(17,"the"),(18,"2008"),(19,"market"),(20,"crash"),(21,".")]


testpt5 = PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Russia")],PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBG","hoping"),PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PN "NP" [PL ("DT","the"),PN "ADJP" [PL ("``","``"),PL ("NN","breakthrough"),PL ("''","''")],PL ("JJ","Syrian"),PL ("NN","ceasefire"),PL ("NN","deal")],PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBD","brokered"),PN "NP-TMP" [PL ("DT","this"),PL ("NN","week")]]]]],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","align"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NNP","U.S.")],PN "PP" [PL ("IN","with"),PN "NP" [PN "NP" [PL ("NNP","President"),PL ("NNP","Vladimir"),PL ("NNP","Putin"),PL ("POS","'s")],PL ("NNS","plans")]]],PN "PP" [PL ("IN","for"),PN "NP" [PL ("DT","the"),PL ("JJ","war-torn"),PL ("NN","count")]]]]]]]],PL (".",".")]]

testlemma5 = [(0,"Russia"),(1,"be"),(2,"hope"),(3,"that"),(4,"the"),(5,"``"),(6,"breakthrough"),(7,"''"),(8,"syrian"),(9,"ceasefire"),(10,"deal"),(11,"it"),(12,"broker"),(13,"this"),(14,"week"),(15,"will"),(16,"align"),(17,"the"),(18,"U.S."),(19,"with"),(20,"President"),(21,"Vladimir"),(22,"Putin"),(23,"'s"),(24,"plan"),(25,"for"),(26,"the"),(27,"war-torn"),(28,"count"),(29,".")]


process :: (PennTree,[(Int,Text)]) -> IO ()
process (testpt,testlemma) = do
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn (T.intercalate " " (map snd (toList testpt)))
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn  . T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList $ testpt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  let lmap1 = IM.fromList (map (_2 %~ Lemma) testlemma)
  showClauseStructure lmap1 testpt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"  
  T.IO.putStrLn $ prettyPrint 0 testpt

  putStrLn "--------------------------------------------------------------------------------------------------------------------"


main :: IO ()
main =
  mapM_ process [ (testpt1,testlemma1)
                , (testpt2,testlemma2)
                , (testpt3,testlemma3)
                , (testpt4,testlemma4)
                , (testpt5,testlemma5)
                ]

