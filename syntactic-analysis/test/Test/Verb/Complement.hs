{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Test.Verb.Complement where

import           Control.Lens               hiding (levels)
import           Data.List                         (find)
import           Data.Maybe                        (fromMaybe,maybeToList)
import           Data.Monoid                       (First(..))
import           Data.Text                         (Text)
import qualified Data.Text                  as T
--
import           Data.Bitree
import           Data.BitreeZipper                 (extractZipperById,toBitree)
import           NLP.Type.NamedEntity
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos
import           WordNet.Type.Lexicographer        (LexicographerFile)
--
import           NLP.Syntax                        (syntacticAnalysis)
import           NLP.Syntax.Clause
import           NLP.Syntax.Type.Resolve           (Resolved(..),Referent(..),referent2CompVP
                                                   ,retrieveResolved,resolvedSpecTP,resolvedCompVP)
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util                   (mkPreAnalysis)
--
import           Test.Common
import           Test.Tasty.HUnit
import           Test.Tasty
--


type TestVerbComplement = (Text
                          ,Int
                          ,((Text,Maybe DetClass),[Text],[Text],Bool)   -- (subject,complements,adjuncts,istopicalized?)
                          ,[(Int,(Lemma,Text))]
                          ,PennTree,[TagPos TokIdx MarkType]
                          ,[(Int,LexicographerFile)]
                          )

main_finite_1 :: TestVerbComplement
main_finite_1
  = ( "He will be fined $25,000.", 3
    , (("",Nothing), ["He","$ 25,000"],[],False)
    , [(0,("he","He")),(1,("will","will")),(2,("be","be")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
    , []
    , []
    )

main_finite_2 :: TestVerbComplement
main_finite_2
  = ( "The move had been widely expected.", 5
    , (("",Nothing), ["The move"],[],False)
    , [(0,("the","The")),(1,("move","move")),(2,("have","had")),(3,("be","been")),(4,("widely","widely")),(5,("expect","expected")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","move")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","been"),PN "VP" [PN "ADVP" [PL ("RB","widely")],PL ("VBN","expected")]]],PL (".",".")]]
    , []
    , []
    )


-- | Reduced relative clause.
rrc_passive_1 :: TestVerbComplement
rrc_passive_1
  = ( "NASA enhances online scientific tool used by hundreds of scientists.", 5
    , (("by hundreds of scientists",Nothing), ["online scientific tool"],[],False)
    , [(0,("NASA","NASA")),(1,("enhance","enhances")),(2,("online","online")),(3,("scientific","scientific")),(4,("tool","tool")),(5,("use","used")),(6,("by","by")),(7,("hundred","hundreds")),(8,("of","of")),(9,("scientist","scientists")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","NASA")],PN "VP" [PL ("VBZ","enhances"),PN "NP" [PN "NP" [PL ("JJ","online"),PL ("JJ","scientific"),PL ("NN","tool")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNS","hundreds")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","scientists")]]]]]]],PL (".",".")]]
    , []
    , []
    )


-- | control movement
inf_control_1 :: TestVerbComplement
inf_control_1
  = ( "Jean is reluctant to leave.", 4
    , (("Jean",Just NoDet), [], [],False)
    , [(0,("Jean","Jean")),(1,("be","is")),(2,("reluctant","reluctant")),(3,("to","to")),(4,("leave","leave")),(5,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Jean")],PN "VP" [PL ("VBZ","is"),PN "ADJP" [PL ("JJ","reluctant"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","leave")]]]]],PL (".",".")]]
    , []
    , []
    )

-- | embedded
embedded_that_1 :: TestVerbComplement
embedded_that_1
  = ( "The cat thinks that he is out of the bag.", 5
    , (("he",Just (Pronoun P_He False)), ["out of the bag"], [],False)
    , [(0,("the","The")),(1,("cat","cat")),(2,("think","thinks")),(3,("that","that")),(4,("he","he")),(5,("be","is")),(6,("out","out")),(7,("of","of")),(8,("the","the")),(9,("bag","bag")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","cat")],PN "VP" [PL ("VBZ","thinks"),PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("VBZ","is"),PN "ADJP" [PL ("IN","out"),PN "PP" [PL ("IN","of"),PN "NP" [PL ("DT","the"),PL ("NN","bag")]]]]]]],PL (".",".")]]
    , []
    , []
    )

-- | restrictive relative clause
restr_rel_1 :: TestVerbComplement
restr_rel_1
  = ( "The guy who is wearing the red hat just hit me!", 4
    , (("The guy",Just (Article Definite)), ["the red hat"], [], False)
    , [(0,("the","The")),(1,("guy","guy")),(2,("who","who")),(3,("be","is")),(4,("wear","wearing")),(5,("the","the")),(6,("red","red")),(7,("hat","hat")),(8,("just","just")),(9,("hit","hit")),(10,("I","me")),(11,("!","!"))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("DT","The"),PL ("NN","guy")],PN "SBAR" [PN "WHNP" [PL ("WP","who")],PN "S" [PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBG","wearing"),PN "NP" [PL ("DT","the"),PL ("JJ","red"),PL ("NN","hat")]]]]]],PN "ADVP" [PL ("RB","just")],PN "VP" [PL ("VBD","hit"),PN "NP" [PL ("PRP","me")]],PL (".","!")]]
    , []
    , []
    )


ditransitive_1 :: TestVerbComplement
ditransitive_1
  = ( "I gave the guy an apple.", 1
    , (("I",Just (Pronoun P_I False)), ["the guy","an apple"], [], False)
    , [(0,("I","I")),(1,("give","gave")),(2,("the","the")),(3,("guy","guy")),(4,("a","an")),(5,("apple","apple")),(6,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","gave"),PN "NP" [PL ("DT","the"),PL ("NN","guy")],PN "NP" [PL ("DT","an"),PL ("NN","apple")]],PL (".",".")]]
    , []
    , []
    )


ditransitive_2 :: TestVerbComplement
ditransitive_2
  = ( "I gave the guy what I got from her.", 1
    , (("I",Just (Pronoun P_I False)), ["the guy", "what I got from her"], [], False)
    , [(0,("I","I")),(1,("give","gave")),(2,("the","the")),(3,("guy","guy")),(4,("what","what")),(5,("I","I")),(6,("get","got")),(7,("from","from")),(8,("she","her")),(9,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","gave"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","guy")],PN "SBAR" [PN "WHNP" [PL ("WDT","what")],PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","got"),PN "PP" [PL ("IN","from"),PN "NP" [PL ("PRP","her")]]]]]]],PL (".",".")]]
    , []
    , []
    )


ditransitive_3 :: TestVerbComplement
ditransitive_3
  = ( "I told you that we would not pass the exam.", 1
    , (("I",Just (Pronoun P_I False)), ["you", "that we would not pass the exam"], [], False)
    , [(0,("I","I")),(1,("tell","told")),(2,("you","you")),(3,("that","that")),(4,("we","we")),(5,("would","would")),(6,("not","not")),(7,("pass","pass")),(8,("the","the")),(9,("exam","exam")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","told"),PN "NP" [PL ("PRP","you")],PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP" [PL ("PRP","we")],PN "VP" [PL ("MD","would"),PL ("RB","not"),PN "VP" [PL ("VB","pass"),PN "NP" [PL ("DT","the"),PL ("NN","exam")]]]]]],PL (".",".")]]
    , []
    , []
    )


ditransitive_4 :: TestVerbComplement
ditransitive_4
  = ( "I told the student he would not pass the exam.", 1
    , (("I",Just (Pronoun P_I False)), ["the student", "he would not pass the exam"], [], False)
    , [(0,("I","I")),(1,("tell","told")),(2,("the","the")),(3,("student","student")),(4,("he","he")),(5,("would","would")),(6,("not","not")),(7,("pass","pass")),(8,("the","the")),(9,("exam","exam")),(10,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","told"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","student")],PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("MD","would"),PL ("RB","not"),PN "VP" [PL ("VB","pass"),PN "NP" [PL ("DT","the"),PL ("NN","exam")]]]]]]],PL (".",".")]]
    , []
    , []
    )

preposedTemporalAdjunct :: TestVerbComplement
preposedTemporalAdjunct
  = ( "Toyota Motor Corp on Monday said it would begin testing self-driving electric cars around 2020."
    , 5
    , (("Toyota Motor Corp",Just NoDet), ["it would begin testing self-driving electric cars around 2020"], ["on Monday"], False)
    , [(0,("Toyota","Toyota")),(1,("Motor","Motor")),(2,("Corp","Corp")),(3,("on","on")),(4,("Monday","Monday")),(5,("say","said")),(6,("it","it")),(7,("would","would")),(8,("begin","begin")),(9,("test","testing")),(10,("self-driving","self-driving")),(11,("electric","electric")),(12,("car","cars")),(13,("around","around")),(14,("2020","2020")),(15,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("NNP","Toyota"),PL ("NNP","Motor"),PL ("NNP","Corp")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Monday")]]],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("MD","would"),PN "VP" [PL ("VB","begin"),PN "S" [PN "VP" [PL ("VBG","testing"),PN "NP" [PL ("JJ","self-driving"),PL ("JJ","electric"),PL ("NNS","cars")],PN "PP" [PL ("IN","around"),PN "NP" [PL ("CD","2020")]]]]]]]]],PL (".",".")]]
    , [TagPos (TokIdx 0,TokIdx 3,MarkEntity Org), TagPos (TokIdx 4, TokIdx 5, MarkTime)]
    , []
    )


adjunctCP_1 :: TestVerbComplement
adjunctCP_1
  = ( "Sansiri would invest $80 million in overseas markets as the Thai real estate developer seeks to expand beyond its core business.", 2
    , (("Sansiri",Just NoDet),["$ 80 million", "overseas markets"], ["as the Thai real estate developer seeks to expand beyond its core business"], False)
    , [(0,("Sansiri","Sansiri")),(1,("would","would")),(2,("invest","invest")),(3,("$","$")),(4,("80","80")),(5,("million","million")),(6,("in","in")),(7,("overseas","overseas")),(8,("market","markets")),(9,("as","as")),(10,("the","the")),(11,("Thai","Thai")),(12,("real","real")),(13,("estate","estate")),(14,("developer","developer")),(15,("seek","seeks")),(16,("to","to")),(17,("expand","expand")),(18,("beyond","beyond")),(19,("its","its")),(20,("core","core")),(21,("business","business")),(22,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Sansiri")],PN "VP" [PL ("MD","would"),PN "VP" [PL ("VB","invest"),PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","80"),PL ("CD","million")]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("JJ","overseas"),PL ("NNS","markets")]],PN "SBAR" [PL ("IN","as"),PN "S" [PN "NP" [PL ("DT","the"),PL ("NNP","Thai"),PL ("JJ","real"),PL ("NN","estate"),PL ("NN","developer")],PN "VP" [PL ("VBZ","seeks"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","expand"),PN "PP" [PL ("IN","beyond"),PN "NP" [PL ("PRP$","its"),PL ("NN","core"),PL ("NN","business")]]]]]]]]]],PL (".",".")]]
    , []
    , []
    )


adjunctCP_2 :: TestVerbComplement
adjunctCP_2
  = ( "Sansiri Pcl said on Wednesday it would invest $80 million in overseas markets, including a 35 percent stake purchase in a U.S.-based hotel chain, as the Thai real estate developer seeks to expand beyond its core business.", 7
    , (("it",Just (Pronoun P_It False)),["$ 80 million","overseas markets"],["as the Thai real estate developer seeks to expand beyond its core business"], False)
    , [(0,("Sansiri","Sansiri")),(1,("Pcl","Pcl")),(2,("say","said")),(3,("on","on")),(4,("Wednesday","Wednesday")),(5,("it","it")),(6,("would","would")),(7,("invest","invest")),(8,("$","$")),(9,("80","80")),(10,("million","million")),(11,("in","in")),(12,("overseas","overseas")),(13,("market","markets")),(14,(",",",")),(15,("include","including")),(16,("a","a")),(17,("35","35")),(18,("percent","percent")),(19,("stake","stake")),(20,("purchase","purchase")),(21,("in","in")),(22,("a","a")),(23,("u.s.-based","U.S.-based")),(24,("hotel","hotel")),(25,("chain","chain")),(26,(",",",")),(27,("as","as")),(28,("the","the")),(29,("Thai","Thai")),(30,("real","real")),(31,("estate","estate")),(32,("developer","developer")),(33,("seek","seeks")),(34,("to","to")),(35,("expand","expand")),(36,("beyond","beyond")),(37,("its","its")),(38,("core","core")),(39,("business","business")),(40,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Sansiri"),PL ("NNP","Pcl")],PN "VP" [PL ("VBD","said"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Wednesday")]],PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("MD","would"),PN "VP" [PL ("VB","invest"),PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","80"),PL ("CD","million")]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("JJ","overseas"),PL ("NNS","markets")]],PL (",",","),PN "PP" [PL ("VBG","including"),PN "NP" [PN "NP" [PL ("DT","a"),PN "ADJP" [PL ("CD","35"),PL ("NN","percent")],PL ("NN","stake"),PL ("NN","purchase")],PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","a"),PL ("JJ","U.S.-based"),PL ("NN","hotel"),PL ("NN","chain")]]]],PL (",",","),PN "SBAR" [PL ("IN","as"),PN "S" [PN "NP" [PL ("DT","the"),PL ("NNP","Thai"),PL ("JJ","real"),PL ("NN","estate"),PL ("NN","developer")],PN "VP" [PL ("VBZ","seeks"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","expand"),PN "PP" [PL ("IN","beyond"),PN "NP" [PL ("PRP$","its"),PL ("NN","core"),PL ("NN","business")]]]]]]]]]]]]],PL (".",".")]]
    , []
    , []
    )

preposedCP :: TestVerbComplement
preposedCP
  = ( "Brazilian steelmaker Companhia Siderugica Nacional SA plans to sell bonds on international markets in an effort to improve its debt profile, Benjamin Steinbruch, chief executive officer, said on Friday."
    , 29
    , (("Benjamin Steinbruch",Just NoDet), ["Friday", "Brazilian steelmaker Companhia Siderugica Nacional SA plans to sell bonds on international markets in an effort to improve its debt profile"], [], True)
    , [(0,("brazilian","Brazilian")),(1,("steelmaker","steelmaker")),(2,("Companhia","Companhia")),(3,("Siderugica","Siderugica")),(4,("Nacional","Nacional")),(5,("SA","SA")),(6,("plan","plans")),(7,("to","to")),(8,("sell","sell")),(9,("bond","bonds")),(10,("on","on")),(11,("international","international")),(12,("market","markets")),(13,("in","in")),(14,("a","an")),(15,("effort","effort")),(16,("to","to")),(17,("improve","improve")),(18,("its","its")),(19,("debt","debt")),(20,("profile","profile")),(21,(",",",")),(22,("Benjamin","Benjamin")),(23,("Steinbruch","Steinbruch")),(24,(",",",")),(25,("chief","chief")),(26,("executive","executive")),(27,("officer","officer")),(28,(",",",")),(29,("say","said")),(30,("on","on")),(31,("Friday","Friday")),(32,(".","."))]
    , PN "ROOT" [PN "S" [PN "S" [PN "NP" [PL ("JJ","Brazilian"),PL ("NN","steelmaker"),PL ("NNP","Companhia"),PL ("NNP","Siderugica"),PL ("NNP","Nacional"),PL ("NNP","SA")],PN "VP" [PL ("VBZ","plans"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","sell"),PN "NP" [PN "NP" [PL ("NNS","bonds")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("JJ","international"),PL ("NNS","markets")]]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","an"),PL ("NN","effort"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","improve"),PN "NP" [PL ("PRP$","its"),PL ("NN","debt"),PL ("NN","profile")]]]]]]]]]]],PL (",",","),PN "NP" [PN "NP" [PL ("NNP","Benjamin"),PL ("NNP","Steinbruch")],PL (",",","),PN "NP" [PL ("JJ","chief"),PL ("NN","executive"),PL ("NN","officer")],PL (",",",")],PN "VP" [PL ("VBD","said"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Friday")]]],PL (".",".")]]
    , [ TagPos (TokIdx 22, TokIdx 24, MarkEntity Person) ]
    , []
    )


complexNP :: TestVerbComplement
complexNP
  = ( "SF Motors Inc, a California-based electric vehicle unit of China's Chongqing Sokon Industry Group Co Ltd, on Thursday said it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $33 million."
    , 21
    , (("SF Motors Inc",Just NoDet), ["it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $ 33 million"], ["on Thursday"], False)
    , [(0,("SF","SF")),(1,("Motors","Motors")),(2,("Inc","Inc")),(3,(",",",")),(4,("a","a")),(5,("california-based","California-based")),(6,("electric","electric")),(7,("vehicle","vehicle")),(8,("unit","unit")),(9,("of","of")),(10,("China","China")),(11,("'s","'s")),(12,("Chongqing","Chongqing")),(13,("Sokon","Sokon")),(14,("Industry","Industry")),(15,("Group","Group")),(16,("Co","Co")),(17,("Ltd","Ltd")),(18,(",",",")),(19,("on","on")),(20,("Thursday","Thursday")),(21,("say","said")),(22,("it","it")),(23,("have","has")),(24,("buy","bought")),(25,("a","an")),(26,("ev","EV")),(27,("and","and")),(28,("battery","battery")),(29,("tech","tech")),(30,("firm","firm")),(31,("head","headed")),(32,("by","by")),(33,("former","former")),(34,("Tesla","Tesla")),(35,("Inc","Inc")),(36,("executive","executive")),(37,("Martin","Martin")),(38,("Eberhard","Eberhard")),(39,("for","for")),(40,("$","$")),(41,("33","33")),(42,("million","million")),(43,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PN "NP" [PL ("NNP","SF"),PL ("NNPS","Motors"),PL ("NNP","Inc")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","California-based"),PL ("JJ","electric"),PL ("NN","vehicle"),PL ("NN","unit")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNP","Chongqing"),PL ("NNP","Sokon"),PL ("NNP","Industry"),PL ("NNP","Group"),PL ("NNP","Co"),PL ("NNP","Ltd")]]],PL (",",",")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Thursday")]]],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","bought"),PN "NP" [PN "NP" [PL ("DT","an"),PL ("NN","EV"),PL ("CC","and"),PL ("NN","battery"),PL ("NN","tech"),PL ("NN","firm")],PN "VP" [PL ("VBN","headed"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("JJ","former"),PL ("NNP","Tesla"),PL ("NNP","Inc"),PL ("NN","executive"),PL ("NNP","Martin"),PL ("NNP","Eberhard")],PN "PP" [PL ("IN","for"),PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","33"),PL ("CD","million")]]]]]]]]]]]],PL (".",".")]]
    , [TagPos (TokIdx 0, TokIdx 3, MarkEntity Org), TagPos (TokIdx 20, TokIdx 21, MarkTime)]
    , []
    )


complexNP_SRParser :: TestVerbComplement
complexNP_SRParser
  = ( "SF Motors Inc, a California-based electric vehicle unit of China's Chongqing Sokon Industry Group Co Ltd, on Thursday said it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $33 million."
    , 21
    , (("SF Motors Inc",Just NoDet), ["it has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $ 33 million"], ["on Thursday"], False)
    , [(0,("SF","SF")),(1,("Motors","Motors")),(2,("Inc","Inc")),(3,(",",",")),(4,("a","a")),(5,("california-based","California-based")),(6,("electric","electric")),(7,("vehicle","vehicle")),(8,("unit","unit")),(9,("of","of")),(10,("China","China")),(11,("'s","'s")),(12,("Chongqing","Chongqing")),(13,("Sokon","Sokon")),(14,("Industry","Industry")),(15,("Group","Group")),(16,("Co","Co")),(17,("Ltd","Ltd")),(18,(",",",")),(19,("on","on")),(20,("Thursday","Thursday")),(21,("say","said")),(22,("it","it")),(23,("have","has")),(24,("buy","bought")),(25,("a","an")),(26,("ev","EV")),(27,("and","and")),(28,("battery","battery")),(29,("tech","tech")),(30,("firm","firm")),(31,("head","headed")),(32,("by","by")),(33,("former","former")),(34,("Tesla","Tesla")),(35,("Inc","Inc")),(36,("executive","executive")),(37,("Martin","Martin")),(38,("Eberhard","Eberhard")),(39,("for","for")),(40,("$","$")),(41,("33","33")),(42,("million","million")),(43,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PN "NP" [PL ("NNP","SF"),PL ("NNPS","Motors"),PL ("NNP","Inc")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","California-based"),PL ("JJ","electric"),PL ("NN","vehicle"),PL ("NN","unit")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNP","Chongqing"),PL ("NNP","Sokon"),PL ("NNP","Industry"),PL ("NNP","Group"),PL ("NNP","Co"),PL ("NNP","Ltd")]]],PL (",",",")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Thursday")]]],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","bought"),PN "NP" [PN "NP" [PL ("DT","an"),PL ("NN","EV"),PL ("CC","and"),PL ("NN","battery"),PL ("NN","tech"),PL ("NN","firm")],PN "VP" [PL ("VBN","headed"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("JJ","former"),PL ("NNP","Tesla"),PL ("NNP","Inc"),PL ("NN","executive"),PL ("NNP","Martin"),PL ("NNP","Eberhard")]]]],PN "PP" [PL ("IN","for"),PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","33"),PL ("CD","million")]]]]]]]],PL (".",".")]]
    , [TagPos (TokIdx 0, TokIdx 3, MarkEntity Org), TagPos (TokIdx 20, TokIdx 21, MarkTime), TagPos (TokIdx 37,TokIdx 39, MarkEntity Person)]
    , []
    )


complexNP_2 :: TestVerbComplement
complexNP_2
  = ( "The event was the first initial public offering on the country's stock exchange."
    , 2
    , (("The event",Just (Article Definite)), ["the first initial public offering"],[], False)
    , [(0,("the","The")),(1,("event","event")),(2,("be","was")),(3,("the","the")),(4,("first","first")),(5,("initial","initial")),(6,("public","public")),(7,("offering","offering")),(8,("on","on")),(9,("the","the")),(10,("country","country")),(11,("'s","'s")),(12,("stock","stock")),(13,("exchange","exchange")),(14,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","event")],PN "VP" [PL ("VBD","was"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("JJ","first"),PL ("JJ","initial"),PL ("JJ","public"),PL ("NN","offering")],PN "PP" [PL ("IN","on"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","country"),PL ("POS","'s")],PL ("NN","stock"),PL ("NN","exchange")]]]],PL (".",".")]]
    , []
    , []
    )

prepComplement :: TestVerbComplement
prepComplement
  = ( "Virgin Group has invested in the company."
    , 3
    , (("Virgin Group",Just NoDet), ["the company"], [], False)
    , [(0,("Virgin","Virgin")),(1,("Group","Group")),(2,("have","has")),(3,("invest","invested")),(4,("in","in")),(5,("the","the")),(6,("company","company")),(7,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","Virgin"),PL ("NNP","Group")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","invested"),PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","the"),PL ("NN","company")]]]],PL (".",".")]]
    , [TagPos (TokIdx 0, TokIdx 2, MarkTime)]
    , []
    )

rrc_passive_2 :: TestVerbComplement
rrc_passive_2
  = ( "SF Motors Inc has bought an EV and battery tech firm headed by former Tesla Inc executive Martin Eberhard for $33 million."
    , 4
    , (("SF Motors Inc",Just NoDet), ["an EV and battery tech firm", "$ 33 million"], [], False)
    , [(0,("SF","SF")),(1,("Motors","Motors")),(2,("Inc","Inc")),(3,("have","has")),(4,("buy","bought")),(5,("a","an")),(6,("ev","EV")),(7,("and","and")),(8,("battery","battery")),(9,("tech","tech")),(10,("firm","firm")),(11,("head","headed")),(12,("by","by")),(13,("former","former")),(14,("Tesla","Tesla")),(15,("Inc","Inc")),(16,("executive","executive")),(17,("Martin","Martin")),(18,("Eberhard","Eberhard")),(19,("for","for")),(20,("$","$")),(21,("33","33")),(22,("million","million")),(23,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","SF"),PL ("NNPS","Motors"),PL ("NNP","Inc")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","bought"),PN "NP" [PN "NP" [PL ("DT","an"),PL ("NN","EV"),PL ("CC","and"),PL ("NN","battery"),PL ("NN","tech"),PL ("NN","firm")],PN "VP" [PL ("VBN","headed"),PN "PP" [PL ("IN","by"),PN "NP" [PL ("JJ","former"),PL ("NNP","Tesla"),PL ("NNP","Inc"),PL ("NN","executive"),PL ("NNP","Martin"),PL ("NNP","Eberhard")]]]],PN "PP" [PL ("IN","for"),PN "NP" [PN "QP" [PL ("$","$"),PL ("CD","33"),PL ("CD","million")]]]]],PL (".",".")]]
    , [TagPos (TokIdx 0, TokIdx 3, MarkEntity Org), TagPos (TokIdx 17,TokIdx 19, MarkEntity Person) ]
    , []
    )


to_infinitive_1 :: TestVerbComplement
to_infinitive_1
  = ( "Indian fashion and retail conglomerate Future Group said on Tuesday it plans to open 10,000 member-only stores by 2022."
    , 11
    , (("it", Just (Pronoun P_It False)), ["to open 10,000 member-only stores by 2022"], [], False)
    , [(0,("indian","Indian")),(1,("fashion","fashion")),(2,("and","and")),(3,("retail","retail")),(4,("conglomerate","conglomerate")),(5,("Future","Future")),(6,("Group","Group")),(7,("say","said")),(8,("on","on")),(9,("Tuesday","Tuesday")),(10,("it","it")),(11,("plan","plans")),(12,("to","to")),(13,("open","open")),(14,("10,000","10,000")),(15,("member-only","member-only")),(16,("store","stores")),(17,("by","by")),(18,("2022","2022")),(19,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PL ("JJ","Indian"),PL ("NN","fashion"),PL ("CC","and"),PL ("JJ","retail"),PL ("NN","conglomerate"),PL ("NNP","Future"),PL ("NNP","Group")],PN "VP" [PL ("VBD","said"),PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Tuesday")]],PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","plans"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","open"),PN "NP" [PL ("CD","10,000"),PL ("JJ","member-only"),PL ("NNS","stores")],PN "PP" [PL ("IN","by"),PN "NP" [PL ("CD","2022")]]]]]]]]],PL (".",".")]]
    , []
    , []
    )


adjp_1 :: TestVerbComplement
adjp_1
  = ( "Shares of U.S. Silica Holdings Inc fell as much as 11 percent on Tuesday after the sand producer reported smaller than expected volume in quarterly sand sales."
    , 18
    , (("the sand producer",Just (Article Definite)), ["smaller than expected volume in quarterly sand sales"],[], False)
    , [(0,("share","Shares")),(1,("of","of")),(2,("U.S.","U.S.")),(3,("Silica","Silica")),(4,("Holdings","Holdings")),(5,("Inc","Inc")),(6,("fall","fell")),(7,("as","as")),(8,("much","much")),(9,("as","as")),(10,("11","11")),(11,("percent","percent")),(12,("on","on")),(13,("Tuesday","Tuesday")),(14,("after","after")),(15,("the","the")),(16,("sand","sand")),(17,("producer","producer")),(18,("report","reported")),(19,("smaller","smaller")),(20,("than","than")),(21,("expect","expected")),(22,("volume","volume")),(23,("in","in")),(24,("quarterly","quarterly")),(25,("sand","sand")),(26,("sale","sales")),(27,(".","."))]
    , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("NNS","Shares")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNP","U.S."),PL ("NNP","Silica"),PL ("NNPS","Holdings"),PL ("NNP","Inc")]]],PN "VP" [PL ("VBD","fell"),PN "NP" [PN "NP" [PN "QP" [PL ("RB","as"),PL ("JJ","much"),PL ("IN","as"),PL ("CD","11")],PL ("NN","percent")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Tuesday")]]],PN "SBAR" [PL ("IN","after"),PN "S" [PN "NP" [PL ("DT","the"),PL ("NN","sand"),PL ("NN","producer")],PN "VP" [PL ("VBD","reported"),PN "ADJP" [PL ("JJR","smaller"),PN "PP" [PL ("IN","than"),PN "NP" [PN "NP" [PL ("VBN","expected"),PL ("NN","volume")],PN "PP" [PL ("IN","in"),PN "NP" [PL ("JJ","quarterly"),PL ("NN","sand"),PL ("NNS","sales")]]]]]]]]],PL (".",".")]]
    , []
    , []
    )

frag_1 :: TestVerbComplement
frag_1 =
  ( "U.S. President Donald Trump on Wednesday dismissed any diplomatic negotiations with North Korea, saying “talking is not the answer,” one day after Pyongyang fired a ballistic missile over Japan and drew international condemnation."
  , 14
  , (("U.S. President Donald Trump",Nothing),["talking is not the answer"],[],False)
  , [(0,("U.S.","U.S.")),(1,("President","President")),(2,("Donald","Donald")),(3,("Trump","Trump")),(4,("on","on")),(5,("Wednesday","Wednesday")),(6,("dismiss","dismissed")),(7,("any","any")),(8,("diplomatic","diplomatic")),(9,("negotiation","negotiations")),(10,("with","with")),(11,("North","North")),(12,("Korea","Korea")),(13,(",",",")),(14,("say","saying")),(15,("``","``")),(16,("talk","talking")),(17,("be","is")),(18,("not","not")),(19,("the","the")),(20,("answer","answer")),(21,(",",",")),(22,("''","''")),(23,("one","one")),(24,("day","day")),(25,("after","after")),(26,("Pyongyang","Pyongyang")),(27,("fire","fired")),(28,("a","a")),(29,("ballistic","ballistic")),(30,("missile","missile")),(31,("over","over")),(32,("Japan","Japan")),(33,("and","and")),(34,("draw","drew")),(35,("international","international")),(36,("condemnation","condemnation")),(37,(".","."))]
  , PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("NNP","U.S."),PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "PP" [PL ("IN","on"),PN "NP" [PL ("NNP","Wednesday")]]],PN "VP" [PL ("VBD","dismissed"),PN "NP" [PN "NP" [PL ("DT","any"),PL ("JJ","diplomatic"),PL ("NNS","negotiations")],PN "PP" [PL ("IN","with"),PN "NP" [PL ("NNP","North"),PL ("NNP","Korea")]]],PL (",",","),PN "S" [PN "VP" [PL ("VBG","saying"),PL ("``","``"),PN "FRAG" [PN "S" [PL ("VBG","talking"),PN "VP" [PL ("VBZ","is"),PL ("RB","not"),PN "NP" [PL ("DT","the"),PL ("NN","answer")]]],PL (",",","),PL ("''","''"),PN "SBAR" [PN "NP" [PL ("CD","one"),PL ("NN","day")],PL ("IN","after"),PN "S" [PN "NP" [PL ("NNP","Pyongyang")],PN "VP" [PN "VP" [PL ("VBD","fired"),PN "NP" [PL ("DT","a"),PL ("JJ","ballistic"),PL ("NN","missile")],PN "PP" [PL ("IN","over"),PN "NP" [PL ("NNP","Japan")]]],PL ("CC","and"),PN "VP" [PL ("VBD","drew"),PN "NP" [PL ("JJ","international"),PL ("NN","condemnation")]]]]]]]]],PL (".",".")]]
  , []
  , []
  )



checkSubjCompAdjunct :: TestVerbComplement -> Bool
checkSubjCompAdjunct c = fromMaybe False $ do
  let 
      lmatknlst = c^._4
      pt = c^._5
      tagposs = c^._6
      synsets = c^._7
      a0 = mkPreAnalysis lmatknlst pt tagposs synsets
      vps = mkVPS lmatknlst pt
      x'trs = syntacticAnalysis a0
  vp <- find (\vp -> vp^.vp_index == (c^._2)) vps
      -- test subjects
  cp0 <- (^._1) <$> constructCP a0 vp   -- seems very inefficient. but mcpstr can have memoized one.
                                         -- anyway need to be rewritten.
  w <- (getFirst . foldMap (First . extractZipperById (cp0^.maximalProjection))) x'trs
  let x'tr = toBitree w
      resmap = retrieveResolved x'tr
  cp <- (currentCPDPPP w) ^? _CPCase
  let subj_test = c^._3._1
      b_subj = fromMaybe False $ do
                 let subj = cp^.complement.specifier
                 let (sclass,stxt) = fromMaybe (Nothing,"") $ do
                       spectp <- resolvedSpecTP resmap subj >>= \case RefRExp x -> return x
                                                                      RefVariable _ (RBound x _)   -> return x
                                                                      RefVariable _ (RFree_WHDP r) -> return (SpecTP_DP r)
                       let scls = do
                             rng_dp <- spectp^?_SpecTP_DP
                             dp <- cpdpppFromX'Tree x'tr rng_dp _DPCase
                             return (dp^.headX.hd_class)
                       return (scls,compVPToHeadText SPH1 a0 x'tr (specTPToCompVP spectp))
                 case subj_test^._2 of
                   Nothing -> return (stxt == subj_test^._1)
                   Just p -> return (stxt == subj_test^._1 && sclass == Just p)
      lst_comps = do
        cvp <- cp^.complement.complement.complement
        r <- referent2CompVP <$> maybeToList (resolvedCompVP resmap cvp)
        return (compVPToHeadText SPH1 a0 x'tr r)
      lst_comps_test = c^._3._2
      b_comps = lst_comps == lst_comps_test
      -- test adjuncts
      lst_adjs = cp^..adjunct.traverse._AdjunctCP_CP.to (T.intercalate " " . tokensByRange a0)
      lst_adjs_test = c^._3._3
      b_adjuncts = lst_adjs == lst_adjs_test
      b_topicalized = fromMaybe False $ do
                        cp^?specifier._Just.coidx_content._SpecCP_Topic
                        return True
  {-
  trace ("\n" ++ show (lst_comps,lst_comps_test)) $ return ()
  trace ("\n" ++ show (lst_adjs,lst_adjs_test)) $ return ()
  trace ("\n" ++ show (b_subj,b_comps,b_adjuncts,b_topicalized == c^._3._4)) $ return () -}
  -- trace ("\n" ++ T.unpack (T.intercalate "\n" (formatDetail (txt,lmatknlst,pt,tagposs,synsets)))) $ return ()

  return  (b_subj && b_comps && b_adjuncts && (b_topicalized == c^._3._4))


testcases :: [TestVerbComplement]
testcases = [ -- -- main_finite_1
              -- -- , main_finite_2
              embedded_that_1
            , restr_rel_1
            , ditransitive_1
            -- -- , ditransitive_2
            , ditransitive_3
            -- -- , ditransitive_4
            -- , preposedTemporalAdjunct
            , preposedCP
            , adjunctCP_1
            , adjunctCP_2
            -- , complexNP
            -- , complexNP_SRParser
            , complexNP_2
            , prepComplement
            -- -- , rrc_passive_1
            , rrc_passive_2
            , to_infinitive_1
            , adjp_1
            -- , frag_1
            ]

unitTests :: TestTree
unitTests = testGroup "Subject and direct/indirect object identification" . flip map testcases $ \c ->
              testCase (T.unpack (c^._1)) $
                (checkSubjCompAdjunct c == True) @? (let (txt,_,_,lmatknlst,pt,tagposs,synsets)=c
                                                     in T.unpack (T.intercalate "\n" (formatDetail  (txt,lmatknlst,pt,tagposs,synsets))))
