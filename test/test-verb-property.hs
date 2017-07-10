{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens               hiding (levels)
import qualified Data.IntMap                as IM
import           Data.Text                         (Text)
import qualified Data.Text                  as T
--
import           CoreNLP.Simple.Type.Simplified
--
import           NLP.Type.PennTreebankII
import           NLP.Type.UniversalDependencies2.Syntax
--
import           SRL.Feature.Verb
import           SRL.Type
import           SRL.Type.Verb
--
import           Test.Tasty.HUnit
import           Test.Tasty


ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,ex14,ex15,ex16,ex17
  :: (Text,(Tense,Aspect,Voice),[(Int,(Lemma,Text))],PennTree,Dependency)
ex1 = ( "He was fined $25,000.", (Past,Simple,Passive)
      , [(0,("he","He")),(1,("be","was")),(2,("fine","fined")),(3,("$","$")),(4,("25,000","25,000")),(5,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBD","was"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]],PL (".",".")]]
      , Dependency 3 [(1,"He"),(2,"was"),(3,"fined"),(4,"$"),(5,"25,000")] [((3,1),NSUBJPASS),((3,2),AUXPASS),((3,5),DOBJ),((5,4),DEP)]
      )


ex2 = ( "He will be fined $25,000.", (Present,Simple,Passive)
      , [(0,("he","He")),(1,("will","will")),(2,("be","be")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]       
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
      , Dependency 4 [(1,"He"),(2,"will"),(3,"be"),(4,"fined"),(5,"$"),(6,"25,000")] [((4,1),NSUBJPASS),((4,2),AUX),((4,3),AUXPASS),((4,6),DOBJ),((6,5),DEP)]
      )



ex3 = ( "He has been fined $25,000.", (Present,Perfect,Passive)
      , [(0,("he","He")),(1,("have","has")),(2,("be","been")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
      , Dependency 4 [(1,"He"),(2,"has"),(3,"been"),(4,"fined"),(5,"$"),(6,"25,000")] [((4,1),NSUBJPASS),((4,2),AUX),((4,3),AUXPASS),((4,6),DOBJ),((6,5),DEP)]
      )



ex4 = ( "The move had been widely expected.", (Past,Perfect,Passive)
      , [(0,("the","The")),(1,("move","move")),(2,("have","had")),(3,("be","been")),(4,("widely","widely")),(5,("expect","expected")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","move")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","been"),PN "VP" [PN "ADVP" [PL ("RB","widely")],PL ("VBN","expected")]]],PL (".",".")]]
      , Dependency 6 [(1,"The"),(2,"move"),(3,"had"),(4,"been"),(5,"widely"),(6,"expected")] [((2,1),DET),((6,2),NSUBJPASS),((6,3),AUX),((6,4),AUXPASS),((6,5),ADVMOD)]
      )

  
ex5 = ( "I am floating.", (Present,Progressive,Active)
      , [(0,("I","I")),(1,("be","am")),(2,("float","floating")),(3,(".","."))]        
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","am"),PN "VP" [PL ("VBG","floating")]],PL (".",".")]]
      , Dependency 3 [(1,"I"),(2,"am"),(3,"floating")] [((3,1),NSUBJ),((3,2),AUX)]
      )


ex6 = ( "I am studying with Maria.", (Present,Progressive,Active)
      , [(0,("I","I")),(1,("be","am")),(2,("study","studying")),(3,("with","with")),(4,("Maria","Maria")),(5,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","am"),PN "VP" [PL ("VBG","studying"),PN "PP" [PL ("IN","with"),PN "NP" [PL ("NNP","Maria")]]]],PL (".",".")]]
      , Dependency 3 [(1,"I"),(2,"am"),(3,"studying"),(4,"with"),(5,"Maria")] [((3,1),NSUBJ),((3,2),AUX),((3,5),NMOD),((5,4),CASE)]
      )



ex7 = ( "We eat lunch.", (Present,Simple,Active)
      , [(0,("we","We")),(1,("eat","eat")),(2,("lunch","lunch")),(3,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","eat"),PN "NP" [PL ("NN","lunch")]],PL (".",".")]]
      , Dependency 2 [(1,"We"),(2,"eat"),(3,"lunch")] [((2,1),NSUBJ),((2,3),DOBJ)]
      )


ex8 = ( "We are eating lunch.", (Present,Progressive,Active)
      , [(0,("we","We")),(1,("be","are")),(2,("eat","eating")),(3,("lunch","lunch")),(4,(".","."))]       
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","are"),PN "VP" [PL ("VBG","eating"),PN "NP" [PL ("NN","lunch")]]],PL (".",".")]]
      , Dependency 3 [(1,"We"),(2,"are"),(3,"eating"),(4,"lunch")] [((3,1),NSUBJ),((3,2),AUX),((3,4),DOBJ)]
      )


ex9 = ( "We are not eating lunch right now.", (Present,Progressive,Active)
      , [(0,("we","We")),(1,("be","are")),(2,("not","not")),(3,("eat","eating")),(4,("lunch","lunch")),(5,("right","right")),(6,("now","now")),(7,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","are"),PL ("RB","not"),PN "VP" [PL ("VBG","eating"),PN "NP" [PL ("NN","lunch")],PN "ADVP" [PL ("RB","right"),PL ("RB","now")]]],PL (".",".")]]
      , Dependency 4 [(1,"We"),(2,"are"),(3,"not"),(4,"eating"),(5,"lunch"),(6,"right"),(7,"now")] [((4,1),NSUBJ),((4,2),AUX),((4,3),NEG),((4,5),DOBJ),((4,7),ADVMOD),((7,6),ADVMOD)]
      )


ex10 = ( "It's not done yet.", (Present,Simple,Passive)
       , [(0,("it","It")),(1,("be","'s")),(2,("not","not")),(3,("do","done")),(4,("yet","yet")),(5,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PL ("RB","not"),PN "VP" [PL ("VBN","done"),PN "ADVP" [PL ("RB","yet")]]],PL (".",".")]]
       , Dependency 4 [(1,"It"),(2,"'s"),(3,"not"),(4,"done"),(5,"yet")] [((4,1),NSUBJPASS),((4,2),AUXPASS),((4,3),NEG),((4,5),ADVMOD)]
       )



ex11 = ( "It's done.", (Present,Simple,Passive)
       , [(0,("it","It")),(1,("be","'s")),(2,("do","done")),(3,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PN "VP" [PL ("VBN","done")]],PL (".",".")]]
       , Dependency 3 [(1,"It"),(2,"'s"),(3,"done")] [((3,1),NSUBJPASS),((3,2),AUXPASS)]
       )


ex12 = ( "It's rarely noted.", (Present,Simple,Passive)
       , [(0,("it","It")),(1,("be","'s")),(2,("rarely","rarely")),(3,("note","noted")),(4,(".","."))] 
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PN "ADVP" [PL ("RB","rarely")],PN "VP" [PL ("VBN","noted")]],PL (".",".")]]
       , Dependency 4 [(1,"It"),(2,"'s"),(3,"rarely"),(4,"noted")] [((4,1),NSUBJPASS),((4,2),AUXPASS),((4,3),ADVMOD)] 
       )



ex13 = ( "I have been watching TV.", (Present,PerfectProgressive,Active)
       , [(0,("I","I")),(1,("have","have")),(2,("be","been")),(3,("watch","watching")),(4,("tv","TV")),(5,(".","."))]         
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBG","watching"),PN "NP" [PL ("NN","TV")]]]],PL (".",".")]]
       , Dependency 4 [(1,"I"),(2,"have"),(3,"been"),(4,"watching"),(5,"TV")] [((4,1),NSUBJ),((4,2),AUX),((4,3),AUX),((4,5),DOBJ)]
       )


ex14 = ( "The book had not been noticed.", (Past,Perfect,Passive)
       , [(0,("the","The")),(1,("book","book")),(2,("have","had")),(3,("not","not")),(4,("be","been")),(5,("notice","noticed")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","book")],PN "VP" [PL ("VBD","had"),PL ("RB","not"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","noticed")]]],PL (".",".")]]
       , Dependency 6 [(1,"The"),(2,"book"),(3,"had"),(4,"not"),(5,"been"),(6,"noticed")] [((2,1),DET),((6,2),NSUBJPASS),((6,3),AUX),((6,4),NEG),((6,5),AUXPASS)]
       )


ex15 = ( "I have done the job.", (Present,Perfect,Active)
       , [(0,("I","I")),(1,("have","have")),(2,("do","done")),(3,("the","the")),(4,("job","job")),(5,(".","."))]         
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")]]],PL (".",".")]]
       , Dependency 3 [(1,"I"),(2,"have"),(3,"done"),(4,"the"),(5,"job")] [((3,1),NSUBJ),((3,2),AUX),((3,5),DOBJ),((5,4),DET)]
       )


ex16 = ( "I haven't done the job.", (Present,Perfect,Active)
       , [(0,("I","I")),(1,("have","have")),(2,("not","n't")),(3,("do","done")),(4,("the","the")),(5,("job","job")),(6,(".","."))]        
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PL ("RB","n't"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")]]],PL (".",".")]]
       , Dependency 4 [(1,"I"),(2,"have"),(3,"n't"),(4,"done"),(5,"the"),(6,"job")] [((4,1),NSUBJ),((4,2),AUX),((4,3),NEG),((4,6),DOBJ),((6,5),DET)]
       )


ex17 = ( "I had done the job at that time.", (Past,Perfect,Active)
       , [(0,("I","I")),(1,("have","had")),(2,("do","done")),(3,("the","the")),(4,("job","job")),(5,("at","at")),(6,("that","that")),(7,("time","time")),(8,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")],PN "PP" [PL ("IN","at"),PN "NP" [PL ("DT","that"),PL ("NN","time")]]]],PL (".",".")]]
       , Dependency 3 [(1,"I"),(2,"had"),(3,"done"),(4,"the"),(5,"job"),(6,"at"),(7,"that"),(8,"time")] [((3,1),NSUBJ),((3,2),AUX),((3,5),DOBJ),((3,8),NMOD),((5,4),DET),((8,6),CASE),((8,7),DET)]
       )


testcases :: [(Text,(Tense,Aspect,Voice),[(Int,(Lemma,Text))],PennTree,Dependency)]
testcases = [ ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,ex13,ex14,ex15,ex16,ex17 ]




checkVP :: (Text,(Tense,Aspect,Voice),[(Int,(Lemma,Text))],PennTree,Dependency) -> Bool
checkVP (txt,expresult,lmatknlst,pt,dep) = -- do
  let lmap= IM.fromList (map (\(i,(l,_)) -> (i,l)) lmatknlst)
      tkmap= IM.fromList (map (\(i,(_,t)) -> (i,t)) lmatknlst)
      vps = verbPropertyFromPennTree lmap pt
  in case vps of
       vp:[] -> expresult == (vp^.vp_tense,vp^.vp_aspect,vp^.vp_voice)
       _ -> False 


unitTestsVerbProperty :: TestTree
unitTestsVerbProperty = testGroup "verb property" . flip map testcases $ \c ->
  testCase (T.unpack (c^._1) ++ show (c^._2)) $ checkVP c @?= True


unitTests :: TestTree
unitTests = testGroup "All Unit tests" [unitTestsVerbProperty]


main :: IO ()
main = defaultMain unitTests

