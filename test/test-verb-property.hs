{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens               hiding (levels)
import qualified Data.IntMap                as IM
import           Data.List                         (find)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
--
import           CoreNLP.Simple.Type.Simplified
--
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Format
import           NLP.Syntax.Type
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import           NLP.Type.UniversalDependencies2.Syntax
--
import           Test.Tasty.HUnit
import           Test.Tasty


-- | (Tense,Aspect,Voice,Maybe Text,Maybe Text) == (tense,aspect,voice,auxiliary,negation)
ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,ex14,ex15,ex16,ex17,ex18,ex19,ex20,ex21,ex22,ex23,ex24,ex25
  :: (Text,Int,(Tense,Aspect,Voice,Maybe Text,Maybe Text),[(Int,(Lemma,Text))],PennTree,Dependency)
ex1 = ( "He was fined $25,000."
      , 2
      , (Past,Simple,Passive,Nothing,Nothing)
      , [(0,("he","He")),(1,("be","was")),(2,("fine","fined")),(3,("$","$")),(4,("25,000","25,000")),(5,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBD","was"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]],PL (".",".")]]
      , Dependency 3 [(1,"He"),(2,"was"),(3,"fined"),(4,"$"),(5,"25,000")] [((3,1),NSUBJPASS),((3,2),AUXPASS),((3,5),DOBJ),((5,4),DEP)]
      )


ex2 = ( "He will be fined $25,000."
      , 3
      , (Present,Simple,Passive, Just "will",Nothing)
      , [(0,("he","He")),(1,("will","will")),(2,("be","be")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]       
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
      , Dependency 4 [(1,"He"),(2,"will"),(3,"be"),(4,"fined"),(5,"$"),(6,"25,000")] [((4,1),NSUBJPASS),((4,2),AUX),((4,3),AUXPASS),((4,6),DOBJ),((6,5),DEP)]
      )



ex3 = ( "He has been fined $25,000."
      , 3
      , (Present,Perfect,Passive,Nothing,Nothing)
      , [(0,("he","He")),(1,("have","has")),(2,("be","been")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
      , Dependency 4 [(1,"He"),(2,"has"),(3,"been"),(4,"fined"),(5,"$"),(6,"25,000")] [((4,1),NSUBJPASS),((4,2),AUX),((4,3),AUXPASS),((4,6),DOBJ),((6,5),DEP)]
      )



ex4 = ( "The move had been widely expected."
      , 5
      , (Past,Perfect,Passive,Nothing,Nothing)
      , [(0,("the","The")),(1,("move","move")),(2,("have","had")),(3,("be","been")),(4,("widely","widely")),(5,("expect","expected")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","move")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","been"),PN "VP" [PN "ADVP" [PL ("RB","widely")],PL ("VBN","expected")]]],PL (".",".")]]
      , Dependency 6 [(1,"The"),(2,"move"),(3,"had"),(4,"been"),(5,"widely"),(6,"expected")] [((2,1),DET),((6,2),NSUBJPASS),((6,3),AUX),((6,4),AUXPASS),((6,5),ADVMOD)]
      )

  
ex5 = ( "I am floating."
      , 2
      , (Present,Progressive,Active,Nothing,Nothing)
      , [(0,("I","I")),(1,("be","am")),(2,("float","floating")),(3,(".","."))]        
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","am"),PN "VP" [PL ("VBG","floating")]],PL (".",".")]]
      , Dependency 3 [(1,"I"),(2,"am"),(3,"floating")] [((3,1),NSUBJ),((3,2),AUX)]
      )


ex6 = ( "I am studying with Maria."
      , 2
      , (Present,Progressive,Active,Nothing,Nothing)
      , [(0,("I","I")),(1,("be","am")),(2,("study","studying")),(3,("with","with")),(4,("Maria","Maria")),(5,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","am"),PN "VP" [PL ("VBG","studying"),PN "PP" [PL ("IN","with"),PN "NP" [PL ("NNP","Maria")]]]],PL (".",".")]]
      , Dependency 3 [(1,"I"),(2,"am"),(3,"studying"),(4,"with"),(5,"Maria")] [((3,1),NSUBJ),((3,2),AUX),((3,5),NMOD),((5,4),CASE)]
      )



ex7 = ( "We eat lunch."
      , 1
      , (Present,Simple,Active,Nothing,Nothing)
      , [(0,("we","We")),(1,("eat","eat")),(2,("lunch","lunch")),(3,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","eat"),PN "NP" [PL ("NN","lunch")]],PL (".",".")]]
      , Dependency 2 [(1,"We"),(2,"eat"),(3,"lunch")] [((2,1),NSUBJ),((2,3),DOBJ)]
      )


ex8 = ( "We are eating lunch."
      , 2
      , (Present,Progressive,Active,Nothing,Nothing)
      , [(0,("we","We")),(1,("be","are")),(2,("eat","eating")),(3,("lunch","lunch")),(4,(".","."))]       
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","are"),PN "VP" [PL ("VBG","eating"),PN "NP" [PL ("NN","lunch")]]],PL (".",".")]]
      , Dependency 3 [(1,"We"),(2,"are"),(3,"eating"),(4,"lunch")] [((3,1),NSUBJ),((3,2),AUX),((3,4),DOBJ)]
      )


ex9 = ( "We are not eating lunch right now."
      , 3 
      , (Present,Progressive,Active,Nothing,Just "not")
      , [(0,("we","We")),(1,("be","are")),(2,("not","not")),(3,("eat","eating")),(4,("lunch","lunch")),(5,("right","right")),(6,("now","now")),(7,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","are"),PL ("RB","not"),PN "VP" [PL ("VBG","eating"),PN "NP" [PL ("NN","lunch")],PN "ADVP" [PL ("RB","right"),PL ("RB","now")]]],PL (".",".")]]
      , Dependency 4 [(1,"We"),(2,"are"),(3,"not"),(4,"eating"),(5,"lunch"),(6,"right"),(7,"now")] [((4,1),NSUBJ),((4,2),AUX),((4,3),NEG),((4,5),DOBJ),((4,7),ADVMOD),((7,6),ADVMOD)]
      )


ex10 = ( "It's not done yet."
       , 3  
       , (Present,Simple,Passive,Nothing,Just "not")
       , [(0,("it","It")),(1,("be","'s")),(2,("not","not")),(3,("do","done")),(4,("yet","yet")),(5,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PL ("RB","not"),PN "VP" [PL ("VBN","done"),PN "ADVP" [PL ("RB","yet")]]],PL (".",".")]]
       , Dependency 4 [(1,"It"),(2,"'s"),(3,"not"),(4,"done"),(5,"yet")] [((4,1),NSUBJPASS),((4,2),AUXPASS),((4,3),NEG),((4,5),ADVMOD)]
       )



ex11 = ( "It's done."
       , 2
       , (Present,Simple,Passive,Nothing,Nothing)
       , [(0,("it","It")),(1,("be","'s")),(2,("do","done")),(3,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PN "VP" [PL ("VBN","done")]],PL (".",".")]]
       , Dependency 3 [(1,"It"),(2,"'s"),(3,"done")] [((3,1),NSUBJPASS),((3,2),AUXPASS)]
       )


ex12 = ( "It's rarely noted."
       , 3
       , (Present,Simple,Passive,Nothing,Nothing)
       , [(0,("it","It")),(1,("be","'s")),(2,("rarely","rarely")),(3,("note","noted")),(4,(".","."))] 
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PN "ADVP" [PL ("RB","rarely")],PN "VP" [PL ("VBN","noted")]],PL (".",".")]]
       , Dependency 4 [(1,"It"),(2,"'s"),(3,"rarely"),(4,"noted")] [((4,1),NSUBJPASS),((4,2),AUXPASS),((4,3),ADVMOD)] 
       )



ex13 = ( "I have been watching TV."
       , 3
       , (Present,PerfectProgressive,Active,Nothing,Nothing)
       , [(0,("I","I")),(1,("have","have")),(2,("be","been")),(3,("watch","watching")),(4,("tv","TV")),(5,(".","."))]         
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBG","watching"),PN "NP" [PL ("NN","TV")]]]],PL (".",".")]]
       , Dependency 4 [(1,"I"),(2,"have"),(3,"been"),(4,"watching"),(5,"TV")] [((4,1),NSUBJ),((4,2),AUX),((4,3),AUX),((4,5),DOBJ)]
       )


ex14 = ( "The book had not been noticed."
       , 5
       , (Past,Perfect,Passive,Nothing,Just "not")
       , [(0,("the","The")),(1,("book","book")),(2,("have","had")),(3,("not","not")),(4,("be","been")),(5,("notice","noticed")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","book")],PN "VP" [PL ("VBD","had"),PL ("RB","not"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","noticed")]]],PL (".",".")]]
       , Dependency 6 [(1,"The"),(2,"book"),(3,"had"),(4,"not"),(5,"been"),(6,"noticed")] [((2,1),DET),((6,2),NSUBJPASS),((6,3),AUX),((6,4),NEG),((6,5),AUXPASS)]
       )


ex15 = ( "I have done the job."
       , 2
       , (Present,Perfect,Active,Nothing,Nothing)
       , [(0,("I","I")),(1,("have","have")),(2,("do","done")),(3,("the","the")),(4,("job","job")),(5,(".","."))]         
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")]]],PL (".",".")]]
       , Dependency 3 [(1,"I"),(2,"have"),(3,"done"),(4,"the"),(5,"job")] [((3,1),NSUBJ),((3,2),AUX),((3,5),DOBJ),((5,4),DET)]
       )


ex16 = ( "I haven't done the job."
       , 3
       , (Present,Perfect,Active,Nothing,Just "not")
       , [(0,("I","I")),(1,("have","have")),(2,("not","n't")),(3,("do","done")),(4,("the","the")),(5,("job","job")),(6,(".","."))]        
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PL ("RB","n't"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")]]],PL (".",".")]]
       , Dependency 4 [(1,"I"),(2,"have"),(3,"n't"),(4,"done"),(5,"the"),(6,"job")] [((4,1),NSUBJ),((4,2),AUX),((4,3),NEG),((4,6),DOBJ),((6,5),DET)]
       )


ex17 = ( "I had done the job at that time."
       , 2
       , (Past,Perfect,Active,Nothing,Nothing)
       , [(0,("I","I")),(1,("have","had")),(2,("do","done")),(3,("the","the")),(4,("job","job")),(5,("at","at")),(6,("that","that")),(7,("time","time")),(8,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")],PN "PP" [PL ("IN","at"),PN "NP" [PL ("DT","that"),PL ("NN","time")]]]],PL (".",".")]]
       , Dependency 3 [(1,"I"),(2,"had"),(3,"done"),(4,"the"),(5,"job"),(6,"at"),(7,"that"),(8,"time")] [((3,1),NSUBJ),((3,2),AUX),((3,5),DOBJ),((3,8),NMOD),((5,4),DET),((8,6),CASE),((8,7),DET)]
       )


ex18 = ( "WhatsApp is being targeted by China's censors."
       , 3
       , (Present,Progressive,Passive,Nothing,Nothing)
       , [(0,("WhatsApp","WhatsApp")),(1,("be","is")),(2,("be","being")),(3,("target","targeted")),(4,("by","by")),(5,("China","China")),(6,("'s","'s")),(7,("censor","censors")),(8,(".","."))]       
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","WhatsApp")],PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBG","being"),PN "VP" [PL ("VBN","targeted"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNS","censors")]]]]],PL (".",".")]]
       , Dependency 4 [(1,"WhatsApp"),(2,"is"),(3,"being"),(4,"targeted"),(5,"by"),(6,"China"),(7,"'s"),(8,"censors")] [((4,1),NSUBJPASS),((4,2),AUX),((4,3),AUXPASS),((4,8),NMOD),((6,7),CASE),((8,5),CASE),((8,6),NMOD)]
       )



ex19 = ( "WhatsApp have been being targeted by China's censors."
       , 4
       , (Present,PerfectProgressive,Passive,Nothing,Nothing)
       , [(0,("WhatsApp","WhatsApp")),(1,("have","have")),(2,("be","been")),(3,("be","being")),(4,("target","targeted")),(5,("by","by")),(6,("China","China")),(7,("'s","'s")),(8,("censor","censors")),(9,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","WhatsApp")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBG","being"),PN "VP" [PL ("VBN","targeted"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNS","censors")]]]]]],PL (".",".")]]
       , Dependency 5 [(1,"WhatsApp"),(2,"have"),(3,"been"),(4,"being"),(5,"targeted"),(6,"by"),(7,"China"),(8,"'s"),(9,"censors")] [((5,1),NSUBJPASS),((5,2),AUX),((5,3),AUX),((5,4),AUXPASS),((5,9),NMOD),((7,8),CASE),((9,6),CASE),((9,7),NMOD)]
       )


ex20 = ( "WhatsApp should not have been being targeted by China's censors."
       , 6
       , (Present,PerfectProgressive,Passive,Just "should",Just "not")
       , [(0,("WhatsApp","WhatsApp")),(1,("should","should")),(2,("not","not")),(3,("have","have")),(4,("be","been")),(5,("be","being")),(6,("target","targeted")),(7,("by","by")),(8,("China","China")),(9,("'s","'s")),(10,("censor","censors")),(11,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","WhatsApp")],PN "VP" [PL ("MD","should"),PL ("RB","not"),PN "VP" [PL ("VB","have"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBG","being"),PN "VP" [PL ("VBN","targeted"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNS","censors")]]]]]]],PL (".",".")]]
       , Dependency 7 [(1,"WhatsApp"),(2,"should"),(3,"not"),(4,"have"),(5,"been"),(6,"being"),(7,"targeted"),(8,"by"),(9,"China"),(10,"'s"),(11,"censors")] [((7,1),NSUBJPASS),((7,2),AUX),((7,3),NEG),((7,4),AUX),((7,5),AUX),((7,6),AUXPASS),((7,11),NMOD),((9,10),CASE),((11,8),CASE),((11,9),NMOD)]
       )

ex21 = ( "I looked at the tree."
       , 1
       , (Past,Simple,Active,Nothing,Nothing)
       , [(0,("I","I")),(1,("look","looked")),(2,("at","at")),(3,("the","the")),(4,("tree","tree")),(5,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","looked"),PN "PP" [PL ("IN","at"),PN "NP" [PL ("DT","the"),PL ("NN","tree")]]],PL (".",".")]]
       , Dependency 2 [(1,"I"),(2,"looked"),(3,"at"),(4,"the"),(5,"tree")] [((2,1),NSUBJ),((2,5),NMOD),((5,3),CASE),((5,4),DET)]
       )


ex22 = ( "He's actively considering a breakup of giant Wall Street banks."
       , 3
       , (Present,Progressive,Active,Nothing,Nothing)
       , [(0,("he","He")),(1,("be","'s")),(2,("actively","actively")),(3,("consider","considering")),(4,("a","a")),(5,("breakup","breakup")),(6,("of","of")),(7,("giant","giant")),(8,("Wall","Wall")),(9,("Street","Street")),(10,("bank","banks")),(11,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBZ","'s"),PN "ADVP" [PL ("RB","actively")],PN "VP" [PL ("VBG","considering"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","breakup")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","giant"),PL ("NNP","Wall"),PL ("NNP","Street"),PL ("NNS","banks")]]]]],PL (".",".")]]
       , Dependency 4 [(1,"He"),(2,"'s"),(3,"actively"),(4,"considering"),(5,"a"),(6,"breakup"),(7,"of"),(8,"giant"),(9,"Wall"),(10,"Street"),(11,"banks")] [((4,1),NSUBJ),((4,2),AUX),((4,3),ADVMOD),((4,6),DOBJ),((6,5),DET),((6,11),NMOD),((11,7),CASE),((11,8),AMOD),((11,9),COMPOUND),((11,10),COMPOUND)]
       )
         

ex23 = ( "A major federal civil rights law does not protect employees from discrimination."
       , 8
       , (Present,Simple,Active,Just "do",Just "not")
       , [(0,("a","A")),(1,("major","major")),(2,("federal","federal")),(3,("civil","civil")),(4,("rights","rights")),(5,("law","law")),(6,("do","does")),(7,("not","not")),(8,("protect","protect")),(9,("employee","employees")),(10,("from","from")),(11,("discrimination","discrimination")),(12,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","A"),PL ("JJ","major"),PL ("JJ","federal"),PL ("JJ","civil"),PL ("NNS","rights"),PL ("NN","law")],PN "VP" [PL ("VBZ","does"),PL ("RB","not"),PN "VP" [PL ("VB","protect"),PN "NP" [PL ("NNS","employees")],PN "PP" [PL ("IN","from"),PN "NP" [PL ("NN","discrimination")]]]],PL (".",".")]]
       , Dependency 9 [(1,"A"),(2,"major"),(3,"federal"),(4,"civil"),(5,"rights"),(6,"law"),(7,"does"),(8,"not"),(9,"protect"),(10,"employees"),(11,"from"),(12,"discrimination")] [((6,1),DET),((6,2),AMOD),((6,3),AMOD),((6,4),AMOD),((6,5),COMPOUND),((9,6),NSUBJ),((9,7),AUX),((9,8),NEG),((9,10),DOBJ),((9,12),NMOD),((12,11),CASE)]
       )
  

ex24 = ( "It isn't done."
       , 3
       , (Present,Simple,Passive,Nothing,Just "not")
       , [(0,("it","It")),(1,("be","is")),(2,("not","n't")),(3,("do","done")),(4,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","is"),PL ("RB","n't"),PN "VP" [PL ("VBN","done")]],PL (".",".")]]
       , Dependency 4 [(1,"It"),(2,"is"),(3,"n't"),(4,"done")] [((4,1),NSUBJPASS),((4,2),AUXPASS),((4,3),NEG)]
       )


-- | Reduced relative clause.
ex25 = ( "NASA enhances online scientific tool used by hundreds of scientists."
       , 5
       , (Present,Simple,Passive,Nothing,Nothing)
       , [(0,("NASA","NASA")),(1,("enhance","enhances")),(2,("online","online")),(3,("scientific","scientific")),(4,("tool","tool")),(5,("use","used")),(6,("by","by")),(7,("hundred","hundreds")),(8,("of","of")),(9,("scientist","scientists")),(10,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","NASA")],PN "VP" [PL ("VBZ","enhances"),PN "NP" [PN "NP" [PL ("JJ","online"),PL ("JJ","scientific"),PL ("NN","tool")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNS","hundreds")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","scientists")]]]]]]],PL (".",".")]]
       , Dependency 2 [(1,"NASA"),(2,"enhances"),(3,"online"),(4,"scientific"),(5,"tool"),(6,"used"),(7,"by"),(8,"hundreds"),(9,"of"),(10,"scientists")] [((2,1),NSUBJ),((2,5),DOBJ),((5,3),AMOD),((5,4),AMOD),((5,6),ACL),((6,8),NMOD),((8,7),CASE),((8,10),NMOD),((10,9),CASE)]
       )

testcases :: [(Text,Int,(Tense,Aspect,Voice,Maybe Text,Maybe Text),[(Int,(Lemma,Text))],PennTree,Dependency)]
testcases = [ ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,ex14,ex15,ex16,ex17,ex18,ex19,ex20,ex21,ex22,ex23,ex24,ex25]



mkVPS :: [(Int,(Lemma,Text))] -> PennTree -> [VerbProperty (BitreeZipperICP '[Lemma])]
mkVPS lmatknlst pt =
  let lemmamap= IM.fromList (map (\(i,(l,_)) -> (i,l)) lmatknlst)
  in verbPropertyFromPennTree lemmamap pt


checkVP :: (Text,Int,(Tense,Aspect,Voice,Maybe Text,Maybe Text),[(Int,(Lemma,Text))],PennTree,Dependency) -> Bool
checkVP (_txt,i,expresult,lmatknlst,pt,_dep) =
  let vps = mkVPS lmatknlst pt
  in case find (\vp -> vp^.vp_index == i) vps of
       Just vp -> expresult == (vp^.vp_tense,vp^.vp_aspect,vp^.vp_voice,vp^?vp_auxiliary._Just._2._2.to unLemma,vp^?vp_negation._Just._2._2.to unLemma)
       _       -> False 


unitTestsVerbProperty :: TestTree
unitTestsVerbProperty = testGroup "verb property" . flip map testcases $ \c ->
  testCase (T.unpack (c^._1) ++ show (c^._3)) $
    (checkVP c == True) @? (show ((mkVPS (c^._4) (c^._5))^..traverse.vp_lemma)  ++ "\n" ++ T.unpack (prettyPrint 0 (c^._5)))
 

unitTests :: TestTree
unitTests = testGroup "All Unit tests" [unitTestsVerbProperty]


main :: IO ()
main = defaultMain unitTests


mainShow :: IO ()
mainShow = do
  flip mapM_ testcases $ \c -> do
    putStrLn "--------------------------------------------------------------------------"
    T.IO.putStrLn (c^._1)
    mapM_ (putStrLn.formatVerbProperty) (mkVPS (c^._4) (c^._5))
