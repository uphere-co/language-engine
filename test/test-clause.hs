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
import           NLP.Syntax.Format
import           NLP.Type.PennTreebankII




test1, test6 :: (Text,[(Int,(Text,Text))],PennTree)
test1 =
  ( "The man, it seems, have a lichtenstein corporation, license in Libya and sheltered in the Bahamas."
   , [(0,("the","The")),(1,("man","man")),(2,(",",",")),(3,("it","it")),(4,("seem","seems")),(5,(",",",")),(6,("have","have")),(7,("a","a")),(8,("lichtenstein","lichtenstein")),(9,("corporation","corporation")),(10,(",",",")),(11,("license","licensed")),(12,("in","in")),(13,("Libya","Libya")),(14,("and","and")),(15,("sheltered","sheltered")),(16,("in","in")),(17,("the","the")),(18,("Bahamas","Bahamas")),(19,(".","."))]    
  , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","man")],PN "PRN" [PL (",",","),PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","seems")]],PL (",",",")],PN "VP" [PL ("VBZ","has"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","Lichtenstein"),PL ("NN","corporation")],PL (",",","),PN "VP" [PL ("VBN","licensed"),PN "PP" [PL ("IN","in"),PN "NP" [PN "NP" [PL ("NNP","Libya")],PL ("CC","and"),PN "NP" [PL ("JJ","sheltered")]]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","the"),PL ("NNPS","Bahamas")]]]]],PL (".",".")]]
  )


test2,test3,test4,test5 :: (Text,[(Int,Text)],PennTree)
test2 =
  ( "President Donald Trump said he's actively considering a breakup of giant Wall Street banks, giving a push to efforts to revive a Depression-era law separating consumer and investment banking."
  , [(0,"President"),(1,"Donald"),(2,"Trump"),(3,"say"),(4,"he"),(5,"be"),(6,"actively"),(7,"consider"),(8,"a"),(9,"breakup"),(10,"of"),(11,"giant"),(12,"Wall"),(13,"Street"),(14,"bank"),(15,","),(16,"give"),(17,"a"),(18,"push"),(19,"to"),(20,"effort"),(21,"to"),(22,"revive"),(23,"a"),(24,"depression-era"),(25,"law"),(26,"separate"),(27,"consumer"),(28,"and"),(29,"investment"),(30,"banking"),(31,".")]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("VBZ","'s"),PN "VP" [PN "ADVP" [PL ("RB","actively")],PN "VP" [PL ("VBG","considering"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","breakup")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","giant"),PL ("NNP","Wall"),PL ("NNP","Street"),PL ("NNS","banks")]]]],PL (",",","),PN "VP" [PL ("VBG","giving"),PN "NP" [PL ("DT","a"),PL ("NN","push")],PN "PP" [PL ("TO","to"),PN "NP" [PL ("NNS","efforts")]],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","revive"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","Depression-era"),PL ("NN","law")],PN "VP" [PL ("VBG","separating"),PN "NP" [PL ("NN","consumer"),PL ("CC","and"),PL ("NN","investment"),PL ("NN","banking")]]]]]]]]]]]],PL (".",".")]]
  )


test3 =
  ( "Carmakers, rideshare services and tech companies are teaming up in an increasingly complex series of aliances."
  , [(0,"carmaker"),(1,","),(2,"rideshare"),(3,"service"),(4,"and"),(5,"tech"),(6,"company"),(7,"be"),(8,"team"),(9,"up"),(10,"in"),(11,"a"),(12,"increasingly"),(13,"complex"),(14,"series"),(15,"of"),(16,"alliance"),(17,".")]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNS","Carmakers"),PL (",",","),PL ("NN","rideshare"),PL ("NNS","services"),PL ("CC","and"),PL ("NN","tech"),PL ("NNS","companies")],PN "VP" [PL ("VBP","are"),PN "VP" [PL ("VBG","teaming"),PN "PRT" [PL ("RP","up")],PN "PP" [PL ("IN","in"),PN "NP" [PN "NP" [PL ("DT","an"),PN "ADJP" [PL ("RB","increasingly"),PL ("JJ","complex")],PL ("NN","series")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","alliances")]]]]]],PL (".",".")]]
  )



test4 =
  ( "But that tenet was undone in 1999, a move that's been blamed by some for the 2008 market crash."
  , [(0,"but"),(1,"that"),(2,"tenet"),(3,"be"),(4,"undo"),(5,"in"),(6,"1999"),(7,","),(8,"a"),(9,"move"),(10,"that"),(11,"be"),(12,"be"),(13,"blame"),(14,"by"),(15,"some"),(16,"for"),(17,"the"),(18,"2008"),(19,"market"),(20,"crash"),(21,".")]
   , PN "ROOT" [PN "S" [PL ("CC","But"),PN "NP" [PL ("DT","that"),PL ("NN","tenet")],PN "VP" [PL ("VBD","was"),PN "VP" [PN "VP" [PL ("VBN","undone"),PN "PP" [PL ("IN","in"),PN "NP" [PL ("CD","1999")]]],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","move")],PN "SBAR" [PN "WHNP" [PL ("WDT","that")],PN "S" [PN "VP" [PL ("VBZ","'s"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","blamed"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("DT","some")],PN "PP" [PL ("IN","for"),PN "NP" [PL ("DT","the"),PL ("CD","2008"),PL ("NN","market"),PL ("NN","crash")]]]]]]]]]]]],PL (".",".")]]
  )


test5 =
  ( "Russia is hoping that the ``breakthrough'' Syrian ceasefire deal it brokered this week will falgin the U.S. with President Vladimir Putin's plans for the war-torn count."
  , [(0,"Russia"),(1,"be"),(2,"hope"),(3,"that"),(4,"the"),(5,"``"),(6,"breakthrough"),(7,"''"),(8,"syrian"),(9,"ceasefire"),(10,"deal"),(11,"it"),(12,"broker"),(13,"this"),(14,"week"),(15,"will"),(16,"align"),(17,"the"),(18,"U.S."),(19,"with"),(20,"President"),(21,"Vladimir"),(22,"Putin"),(23,"'s"),(24,"plan"),(25,"for"),(26,"the"),(27,"war-torn"),(28,"count"),(29,".")]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Russia")],PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBG","hoping"),PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PN "NP" [PL ("DT","the"),PN "ADJP" [PL ("``","``"),PL ("NN","breakthrough"),PL ("''","''")],PL ("JJ","Syrian"),PL ("NN","ceasefire"),PL ("NN","deal")],PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBD","brokered"),PN "NP-TMP" [PL ("DT","this"),PL ("NN","week")]]]]],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","align"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NNP","U.S.")],PN "PP" [PL ("IN","with"),PN "NP" [PN "NP" [PL ("NNP","President"),PL ("NNP","Vladimir"),PL ("NNP","Putin"),PL ("POS","'s")],PL ("NNS","plans")]]],PN "PP" [PL ("IN","for"),PN "NP" [PL ("DT","the"),PL ("JJ","war-torn"),PL ("NN","count")]]]]]]]],PL (".",".")]]
  )




test6 =
  ( "The product enables people to create a new kinds of arts, including electronic music."
  , [(0,("the","The")),(1,("product","product")),(2,("enable","enables")),(3,("people","people")),(4,("to","to")),(5,("create","create")),(6,("a","a")),(7,("new","new")),(8,("kind","kinds")),(9,("of","of")),(10,("art","arts")),(11,(",",",")),(12,("include","including")),(13,("electronic","electronic")),(14,("music","music")),(15,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","product")],PN "VP" [PL ("VBZ","enables"),PN "S" [PN "NP" [PL ("NNS","people")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","create"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","new"),PL ("NNS","kinds")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","arts")]],PL (",",","),PN "PP" [PL ("VBG","including"),PN "NP" [PL ("JJ","electronic"),PL ("NN","music")]]]]]]],PL (".",".")]]
  )

test7 =
  ( "Republican senators plan to write a health-care bill."
  , [(0,("republican","Republican")),(1,("senator","senators")),(2,("plan","plan")),(3,("to","to")),(4,("write","write")),(5,("a","a")),(6,("health-care","health-care")),(7,("bill","bill")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","Republican"),PL ("NNS","senators")],PN "VP" [PL ("VBP","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","health-care"),PL ("NN","bill")]]]]],PL (".",".")]]
  )
  
test8 =
  ( "I want to plan to write a paper."
  , [(0,("I","I")),(1,("want","want")),(2,("to","to")),(3,("plan","plan")),(4,("to","to")),(5,("write","write")),(6,("a","a")),(7,("paper","paper")),(8,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","want"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","plan"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","paper")]]]]]]]],PL (".",".")]]
  )


process :: (Text,[(Int,(Text,Text))],PennTree) -> IO ()
process (txt,lma,pt) = do
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn txt 
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn  . T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList $ pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  let lmap1 = IM.fromList (map (_2 %~ (\x -> Lemma (x^._1)))  lma)
  showClauseStructure lmap1 pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"  
  T.IO.putStrLn $ prettyPrint 0 pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"


main :: IO ()
main = mapM_ process [ test1, test6, test7, test8 ]


