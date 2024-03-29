{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Clause where

import           Control.Lens               hiding (levels)
import           Data.Foldable
import qualified Data.IntMap                as IM
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           Data.Bitree
import           Data.BitreeZipper                 (current,mkBitreeZipper)
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos
--
import           NLP.Syntax.Format
import           NLP.Syntax.Preposition
import           NLP.Syntax.Type                   (MarkType(..))
import           NLP.Syntax.Util
--
import           Test.Common

{-
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
-}




-- | coordination
--
test_coordination :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_coordination =
  ( "Fantasy author Cecilia Tan thought she was a Ravenclaw - then she had to face facts."
  , [(0,("Fantasy","Fantasy")),(1,("author","author")),(2,("Cecilia","Cecilia")),(3,("Tan","Tan")),(4,("think","thought")),(5,("she","she")),(6,("be","was")),(7,("a","a")),(8,("ravenclaw","Ravenclaw")),(9,("-","-")),(10,("then","then")),(11,("she","she")),(12,("have","had")),(13,("to","to")),(14,("face","face")),(15,("fact","facts")),(16,(".","."))]
  , PN "ROOT" [PN "S" [PN "S" [PN "NP" [PL ("NNP","Fantasy"),PL ("NN","author"),PL ("NNP","Cecilia"),PL ("NNP","Tan")],PN "VP" [PL ("VBD","thought"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","she")],PN "VP" [PL ("VBD","was"),PN "NP" [PL ("DT","a"),PL ("NN","Ravenclaw")]]]]]],PL (":","-"),PN "S" [PN "ADVP" [PL ("RB","then")],PN "NP" [PL ("PRP","she")],PN "VP" [PL ("VBD","had"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","face"),PN "NP" [PL ("NNS","facts")]]]]]],PL (".",".")]]
  , []
  )


-- | complex noun phrase
--
test_complex_noun_phrase :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_complex_noun_phrase =
  ( "Brewery executive Kosuke Kuji brought his best sake to a New York booze snowcase 16 years ago hoping to promote high-end Japanese rice wine to a new generation of sophisticated foreign drinkers."
  , [(0,("Brewery","Brewery")),(1,("executive","executive")),(2,("Kosuke","Kosuke")),(3,("Kuji","Kuji")),(4,("bring","brought")),(5,("he","his")),(6,("best","best")),(7,("sake","sake")),(8,("to","to")),(9,("a","a")),(10,("New","New")),(11,("York","York")),(12,("booze","booze")),(13,("snowcase","snowcase")),(14,("16","16")),(15,("year","years")),(16,("ago","ago")),(17,("hope","hoping")),(18,("to","to")),(19,("promote","promote")),(20,("high-end","high-end")),(21,("japanese","Japanese")),(22,("rice","rice")),(23,("wine","wine")),(24,("to","to")),(25,("a","a")),(26,("new","new")),(27,("generation","generation")),(28,("of","of")),(29,("sophisticated","sophisticated")),(30,("foreign","foreign")),(31,("drinker","drinkers")),(32,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Brewery"),PL ("NN","executive"),PL ("NNP","Kosuke"),PL ("NNP","Kuji")],PN "VP" [PL ("VBD","brought"),PN "NP" [PL ("PRP$","his"),PL ("JJS","best"),PL ("NN","sake")],PN "PP" [PL ("TO","to"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NNP","New"),PL ("NNP","York"),PL ("NN","booze"),PL ("NN","snowcase")],PN "VP" [PN "ADVP" [PN "NP" [PL ("CD","16"),PL ("NNS","years")],PL ("IN","ago")],PL ("VBG","hoping"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","promote"),PN "NP" [PL ("JJ","high-end"),PL ("JJ","Japanese"),PL ("NN","rice"),PL ("NN","wine")],PN "PP" [PL ("TO","to"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","new"),PL ("NN","generation")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","sophisticated"),PL ("JJ","foreign"),PL ("NNS","drinkers")]]]]]]]]]]],PL (".",".")]]
  , []
  )



-- | bare noun adverb
--
test_bare_noun_adverb :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_bare_noun_adverb =
  ( "The project began this year."
  , [(0,("the","The")),(1,("project","project")),(2,("begin","began")),(3,("this","this")),(4,("year","year")),(5,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","project")],PN "VP" [PL ("VBD","began"),PN "NP-TMP" [PL ("DT","this"),PL ("NN","year")]],PL (".",".")]]
  , [TagPos (TokIdx {unTokIdx = 3},TokIdx {unTokIdx = 5},MarkTime)]
  )




-- | relative WH-pronoun subject linking
--
test_relative_pronoun_subject :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_relative_pronoun_subject =
  ( "I saw the man who sat on the bench."
  -- , 5, (Subj,TraceChain [Moved,WHPRO] (Just "the man"))
  , [(0,("I","I")),(1,("see","saw")),(2,("the","the")),(3,("man","man")),(4,("who","who")),(5,("sit","sat")),(6,("on","on")),(7,("the","the")),(8,("bench","bench")),(9,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","saw"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","man")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBD","sat"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("DT","the"),PL ("NN","bench")]]]]]]],PL (".",".")]]
  , []
  )


-- |
test_noun_modifier :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_noun_modifier =
  ( "Billionaire environmentalist Tom Steyer said the idea is a political pipe dream anyway."
  , [(0,("billionaire","Billionaire")),(1,("environmentalist","environmentalist")),(2,("Tom","Tom")),(3,("Steyer","Steyer")),(4,("say","said")),(5,("the","the")),(6,("idea","idea")),(7,("be","is")),(8,("a","a")),(9,("political","political")),(10,("pipe","pipe")),(11,("dream","dream")),(12,("anyway","anyway")),(13,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NN","Billionaire"),PL ("NN","environmentalist"),PL ("NNP","Tom"),PL ("NNP","Steyer")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("DT","the"),PL ("NN","idea")],PN "VP" [PL ("VBZ","is"),PN "NP" [PL ("DT","a"),PL ("JJ","political"),PL ("NN","pipe"),PL ("NN","dream")],PN "ADVP" [PL ("RB","anyway")]]]]],PL (".",".")]]
  , [TagPos (TokIdx {unTokIdx = 2},TokIdx {unTokIdx = 4},MarkEntity)]
  )

-- |
test_sub_clause :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_sub_clause =
  ( "I ran after the rain had stopped."
  , [(0,("I","I")),(1,("run","ran")),(2,("after","after")),(3,("the","the")),(4,("rain","rain")),(5,("have","had")),(6,("stop","stopped")),(7,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","ran"),PN "SBAR" [PL ("IN","after"),PN "S" [PN "NP" [PL ("DT","the"),PL ("NN","rain")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","stopped")]]]]],PL (".",".")]]
  , []
  )


-- |
test_gerundive :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType])
test_gerundive =
  ( "Travis Kalanick said he had appointed two new board members, challenging Uber shareholders."
  , [(0,("Travis","Travis")),(1,("Kalanick","Kalanick")),(2,("say","said")),(3,("he","he")),(4,("have","had")),(5,("appoint","appointed")),(6,("two","two")),(7,("new","new")),(8,("board","board")),(9,("member","members")),(10,(",",",")),(11,("challenge","challenging")),(12,("Uber","Uber")),(13,("shareholder","shareholders")),(14,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Travis"),PL ("NNP","Kalanick")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","appointed"),PN "NP" [PL ("CD","two"),PL ("JJ","new"),PL ("NN","board"),PL ("NNS","members")],PL (",",","),PN "S" [PN "VP" [PL ("VBG","challenging"),PN "NP" [PL ("NNP","Uber"),PL ("NNS","shareholders")]]]]]]]],PL (".",".")]]
  , []
  )


-- |
test_temporal =
  ( "Kenyan stocks and bonds fell on Wednesday after opposition leader Raila Odinga pulled out of a repeat presidential election set for Oct. 26."
  , [(0,("kenyan","Kenyan")),(1,("stock","stocks")),(2,("and","and")),(3,("bond","bonds")),(4,("fall","fell")),(5,("on","on")),(6,("Wednesday","Wednesday")),(7,("after","after")),(8,("opposition","opposition")),(9,("leader","leader")),(10,("Raila","Raila")),(11,("Odinga","Odinga")),(12,("pull","pulled")),(13,("out","out")),(14,("of","of")),(15,("a","a")),(16,("repeat","repeat")),(17,("presidential","presidential")),(18,("election","election")),(19,("set","set")),(20,("for","for")),(21,("Oct.","Oct.")),(22,("26","26")),(23,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","Kenyan"),PL ("NNS","stocks"),PL ("CC","and"),PL ("NNS","bonds")],PN "VP" [PL ("VBD","fell"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Wednesday")]],PN "SBAR" [PL ("IN","after"),PN "S" [PN "NP" [PL ("NN","opposition"),PL ("NN","leader"),PL ("NNP","Raila"),PL ("NNP","Odinga")],PN "VP" [PL ("VBD","pulled"),PN "PRT" [PL ("RP","out")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","repeat"),PL ("JJ","presidential"),PL ("NN","election")],PN "VP" [PL ("VBN","set"),PN "PP" [PL ("IN","for"),PN "NP" [PL ("NNP","Oct."),PL ("CD","26")]]]]]]]]],PL (".",".")]]
  , [TagPos (TokIdx 6,TokIdx 7,MarkTime)]
  )

test_pp_gerund =
  ( "It was the plan in targeting the league on taxes."
  , [(0,("it","It")),(1,("be","was")),(2,("the","the")),(3,("plan","plan")),(4,("in","in")),(5,("target","targeting")),(6,("the","the")),(7,("league","league")),(8,("on","on")),(9,("tax","taxes")),(10,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBD","was"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","plan")],PN "PP" [PL ("IN","in"),PN "S" [PN "VP" [PL ("VBG","targeting"),PN "NP" [PL ("DT","the"),PL ("NN","league")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNS","taxes")]]]]]]],PL (".",".")]]
  , []
  )
  


-- |
{-
test_nonrestrictive_relative_clause =
  ( "Toyota said it would begin testing self-driving electric cars around 2020, which will use artificial intelligence to engage with drivers."
  , [(0,("Toyota","Toyota")),(1,("say","said")),(2,("it","it")),(3,("would","would")),(4,("begin","begin")),(5,("test","testing")),(6,("self-driving","self-driving")),(7,("electric","electric")),(8,("car","cars")),(9,("around","around")),(10,("2020","2020")),(11,(",",",")),(12,("which","which")),(13,("will","will")),(14,("use","use")),(15,("artificial","artificial")),(16,("intelligence","intelligence")),(17,("to","to")),(18,("engage","engage")),(19,("with","with")),(20,("driver","drivers")),(21,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Toyota")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("MD","would"),PN "VP" [PL ("VB","begin"),PN "S" [PN "VP" [PL ("VBG","testing"),PN "NP" [PL ("JJ","self-driving"),PL ("JJ","electric"),PL ("NNS","cars")],PN "PP" [PL ("IN","around"),PN "NP" [PN "NP" [PL ("CD","2020")],PL (",",","),PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","use"),PN "NP" [PL ("JJ","artificial"),PL ("NN","intelligence")],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","engage"),PN "PP" [PL ("IN","with"),PN "NP" [PL ("NNS","drivers")]]]]]]]]]]]]]]]]]],PL (".",".")]]
  , []
  )
-}

{- 
showDetail :: (Text,[(Int,(Lemma,Text))],PennTree,[TagPos TokIdx MarkType]) -> IO ()
showDetail (txt,lma,pt,tmxs) = do
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn txt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn  . T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip ([0..] :: [Int]) . map snd . toList $ pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  let lmap1 = IM.fromList (map (_2 %~ (^._1)) lma)
  showClauseStructure tmxs lmap1 pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  T.IO.putStrLn $ prettyPrint 0 pt
  putStrLn "--------------------------------------------------------------------------------------------------------------------"
  flip mapM_ tmxs $ \(TagPos (b,e,_tag)) -> do
    let lemmapt = mkBitreeICP lmap1 pt
        rng = beginEndToRange (b,e)
    case find (\z -> getRoot (current z) ^? _Left . _1  == Just rng) $ getNodes (mkBitreeZipper [] lemmapt) of
      Nothing -> return ()
      Just z -> print $ hasEmptyPreposition z

-}

showDetail = T.IO.putStrLn . T.intercalate "\n" . formatDetail

mainShow :: IO ()
mainShow = mapM_ showDetail [ test_coordination
                            , test_complex_noun_phrase
                            , test_bare_noun_adverb
                            , test_relative_pronoun_subject
                            , test_noun_modifier
                            , test_sub_clause
                            , test_gerundive
                            , test_temporal
                            -- -- , test_nonrestrictive_relative_clause
                            , test_pp_gerund
                            ]
