{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Verb.Property where

import           Control.Lens               hiding (levels)
import           Data.List                         (find, intercalate)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Text.Printf
--
import           Data.Bitree                       (getRoot)
import           Data.BitreeZipper                 (current)
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty
--
import           NLP.Syntax.Format
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
--
import           Test.Common
import           Test.Tasty.HUnit
import           Test.Tasty


type TestVerbProp = (Text,Int,(Tense,Aspect,Voice,[Text],Maybe Text),[(Int,(Lemma,Text))],PennTree)

-- | (Tense,Aspect,Voice,Maybe Text,Maybe Text) == (tense,aspect,voice,auxiliary,negation)
--
ex1 ,ex2 ,ex3 ,ex4 ,ex5 ,ex6 ,ex7 ,ex8 ,ex9 ,ex10 :: TestVerbProp
ex11,ex12,ex13,ex14,ex15,ex16,ex17,ex18,ex19,ex20 :: TestVerbProp
ex21,ex22,ex23,ex24,ex25,ex26,ex27,ex28,ex29,ex30 :: TestVerbProp
ex31,ex32,ex33                                    :: TestVerbProp


ex1 = ( "He was fined $25,000."
      , 2
      , (Past,Simple,Passive,[],Nothing)
      , [(0,("he","He")),(1,("be","was")),(2,("fine","fined")),(3,("$","$")),(4,("25,000","25,000")),(5,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBD","was"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]],PL (".",".")]]
      )


ex2 = ( "He will be fined $25,000."
      , 3
      , (Present,Simple,Passive, ["will"],Nothing)
      , [(0,("he","He")),(1,("will","will")),(2,("be","be")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","be"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
      )



ex3 = ( "He has been fined $25,000."
      , 3
      , (Present,Perfect,Passive,[],Nothing)
      , [(0,("he","He")),(1,("have","has")),(2,("be","been")),(3,("fine","fined")),(4,("$","$")),(5,("25,000","25,000")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","fined"),PN "NP" [PL ("$","$"),PL ("CD","25,000")]]]],PL (".",".")]]
      )



ex4 = ( "The move had been widely expected."
      , 5
      , (Past,Perfect,Passive,[],Nothing)
      , [(0,("the","The")),(1,("move","move")),(2,("have","had")),(3,("be","been")),(4,("widely","widely")),(5,("expect","expected")),(6,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","move")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","been"),PN "VP" [PN "ADVP" [PL ("RB","widely")],PL ("VBN","expected")]]],PL (".",".")]]
      )


ex5 = ( "I am floating."
      , 2
      , (Present,Progressive,Active,[],Nothing)
      , [(0,("I","I")),(1,("be","am")),(2,("float","floating")),(3,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","am"),PN "VP" [PL ("VBG","floating")]],PL (".",".")]]
      )


ex6 = ( "I am studying with Maria."
      , 2
      , (Present,Progressive,Active,[],Nothing)
      , [(0,("I","I")),(1,("be","am")),(2,("study","studying")),(3,("with","with")),(4,("Maria","Maria")),(5,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","am"),PN "VP" [PL ("VBG","studying"),PN "PP" [PL ("IN","with"),PN "NP" [PL ("NNP","Maria")]]]],PL (".",".")]]
      )



ex7 = ( "We eat lunch."
      , 1
      , (Present,Simple,Active,[],Nothing)
      , [(0,("we","We")),(1,("eat","eat")),(2,("lunch","lunch")),(3,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","eat"),PN "NP" [PL ("NN","lunch")]],PL (".",".")]]
      )


ex8 = ( "We are eating lunch."
      , 2
      , (Present,Progressive,Active,[],Nothing)
      , [(0,("we","We")),(1,("be","are")),(2,("eat","eating")),(3,("lunch","lunch")),(4,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","are"),PN "VP" [PL ("VBG","eating"),PN "NP" [PL ("NN","lunch")]]],PL (".",".")]]
      )


ex9 = ( "We are not eating lunch right now."
      , 3
      , (Present,Progressive,Active,[],Just "not")
      , [(0,("we","We")),(1,("be","are")),(2,("not","not")),(3,("eat","eating")),(4,("lunch","lunch")),(5,("right","right")),(6,("now","now")),(7,(".","."))]
      , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","We")],PN "VP" [PL ("VBP","are"),PL ("RB","not"),PN "VP" [PL ("VBG","eating"),PN "NP" [PL ("NN","lunch")],PN "ADVP" [PL ("RB","right"),PL ("RB","now")]]],PL (".",".")]]
      )


ex10 = ( "It's not done yet."
       , 3
       , (Present,Simple,Passive,[],Just "not")
       , [(0,("it","It")),(1,("be","'s")),(2,("not","not")),(3,("do","done")),(4,("yet","yet")),(5,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PL ("RB","not"),PN "VP" [PL ("VBN","done"),PN "ADVP" [PL ("RB","yet")]]],PL (".",".")]]
       )



ex11 = ( "It's done."
       , 2
       , (Present,Simple,Passive,[],Nothing)
       , [(0,("it","It")),(1,("be","'s")),(2,("do","done")),(3,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PN "VP" [PL ("VBN","done")]],PL (".",".")]]
       )


ex12 = ( "It's rarely noted."
       , 3
       , (Present,Simple,Passive,[],Nothing)
       , [(0,("it","It")),(1,("be","'s")),(2,("rarely","rarely")),(3,("note","noted")),(4,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","'s"),PN "ADVP" [PL ("RB","rarely")],PN "VP" [PL ("VBN","noted")]],PL (".",".")]]
       )



ex13 = ( "I have been watching TV."
       , 3
       , (Present,PerfectProgressive,Active,[],Nothing)
       , [(0,("I","I")),(1,("have","have")),(2,("be","been")),(3,("watch","watching")),(4,("tv","TV")),(5,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBG","watching"),PN "NP" [PL ("NN","TV")]]]],PL (".",".")]]
       )


ex14 = ( "The book had not been noticed."
       , 5
       , (Past,Perfect,Passive,[],Just "not")
       , [(0,("the","The")),(1,("book","book")),(2,("have","had")),(3,("not","not")),(4,("be","been")),(5,("notice","noticed")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","book")],PN "VP" [PL ("VBD","had"),PL ("RB","not"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBN","noticed")]]],PL (".",".")]]
       )


ex15 = ( "I have done the job."
       , 2
       , (Present,Perfect,Active,[],Nothing)
       , [(0,("I","I")),(1,("have","have")),(2,("do","done")),(3,("the","the")),(4,("job","job")),(5,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")]]],PL (".",".")]]
       )


ex16 = ( "I haven't done the job."
       , 3
       , (Present,Perfect,Active,[],Just "not")
       , [(0,("I","I")),(1,("have","have")),(2,("not","n't")),(3,("do","done")),(4,("the","the")),(5,("job","job")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PL ("RB","n't"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")]]],PL (".",".")]]
       )


ex17 = ( "I had done the job at that time."
       , 2
       , (Past,Perfect,Active,[],Nothing)
       , [(0,("I","I")),(1,("have","had")),(2,("do","done")),(3,("the","the")),(4,("job","job")),(5,("at","at")),(6,("that","that")),(7,("time","time")),(8,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","had"),PN "VP" [PL ("VBN","done"),PN "NP" [PL ("DT","the"),PL ("NN","job")],PN "PP" [PL ("IN","at"),PN "NP" [PL ("DT","that"),PL ("NN","time")]]]],PL (".",".")]]
       )


ex18 = ( "WhatsApp is being targeted by China's censors."
       , 3
       , (Present,Progressive,Passive,[],Nothing)
       , [(0,("WhatsApp","WhatsApp")),(1,("be","is")),(2,("be","being")),(3,("target","targeted")),(4,("by","by")),(5,("China","China")),(6,("'s","'s")),(7,("censor","censors")),(8,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","WhatsApp")],PN "VP" [PL ("VBZ","is"),PN "VP" [PL ("VBG","being"),PN "VP" [PL ("VBN","targeted"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNS","censors")]]]]],PL (".",".")]]
       )



ex19 = ( "WhatsApp have been being targeted by China's censors."
       , 4
       , (Present,PerfectProgressive,Passive,[],Nothing)
       , [(0,("WhatsApp","WhatsApp")),(1,("have","have")),(2,("be","been")),(3,("be","being")),(4,("target","targeted")),(5,("by","by")),(6,("China","China")),(7,("'s","'s")),(8,("censor","censors")),(9,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","WhatsApp")],PN "VP" [PL ("VBP","have"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBG","being"),PN "VP" [PL ("VBN","targeted"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNS","censors")]]]]]],PL (".",".")]]
       )


ex20 = ( "WhatsApp should not have been being targeted by China's censors."
       , 6
       , (Present,PerfectProgressive,Passive,["should"],Just "not")
       , [(0,("WhatsApp","WhatsApp")),(1,("should","should")),(2,("not","not")),(3,("have","have")),(4,("be","been")),(5,("be","being")),(6,("target","targeted")),(7,("by","by")),(8,("China","China")),(9,("'s","'s")),(10,("censor","censors")),(11,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","WhatsApp")],PN "VP" [PL ("MD","should"),PL ("RB","not"),PN "VP" [PL ("VB","have"),PN "VP" [PL ("VBN","been"),PN "VP" [PL ("VBG","being"),PN "VP" [PL ("VBN","targeted"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNP","China"),PL ("POS","'s")],PL ("NNS","censors")]]]]]]],PL (".",".")]]
       )

ex21 = ( "I looked at the tree."
       , 1
       , (Past,Simple,Active,[],Nothing)
       , [(0,("I","I")),(1,("look","looked")),(2,("at","at")),(3,("the","the")),(4,("tree","tree")),(5,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","looked"),PN "PP" [PL ("IN","at"),PN "NP" [PL ("DT","the"),PL ("NN","tree")]]],PL (".",".")]]
       )


ex22 = ( "He's actively considering a breakup of giant Wall Street banks."
       , 3
       , (Present,Progressive,Active,[],Nothing)
       , [(0,("he","He")),(1,("be","'s")),(2,("actively","actively")),(3,("consider","considering")),(4,("a","a")),(5,("breakup","breakup")),(6,("of","of")),(7,("giant","giant")),(8,("Wall","Wall")),(9,("Street","Street")),(10,("bank","banks")),(11,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","He")],PN "VP" [PL ("VBZ","'s"),PN "ADVP" [PL ("RB","actively")],PN "VP" [PL ("VBG","considering"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","breakup")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","giant"),PL ("NNP","Wall"),PL ("NNP","Street"),PL ("NNS","banks")]]]]],PL (".",".")]]
       )


ex23 = ( "A major federal civil rights law does not protect employees from discrimination."
       , 8
       , (Present,Simple,Active,["do"],Just "not")
       , [(0,("a","A")),(1,("major","major")),(2,("federal","federal")),(3,("civil","civil")),(4,("rights","rights")),(5,("law","law")),(6,("do","does")),(7,("not","not")),(8,("protect","protect")),(9,("employee","employees")),(10,("from","from")),(11,("discrimination","discrimination")),(12,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","A"),PL ("JJ","major"),PL ("JJ","federal"),PL ("JJ","civil"),PL ("NNS","rights"),PL ("NN","law")],PN "VP" [PL ("VBZ","does"),PL ("RB","not"),PN "VP" [PL ("VB","protect"),PN "NP" [PL ("NNS","employees")],PN "PP" [PL ("IN","from"),PN "NP" [PL ("NN","discrimination")]]]],PL (".",".")]]
       )


ex24 = ( "It isn't done."
       , 3
       , (Present,Simple,Passive,[],Just "not")
       , [(0,("it","It")),(1,("be","is")),(2,("not","n't")),(3,("do","done")),(4,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","It")],PN "VP" [PL ("VBZ","is"),PL ("RB","n't"),PN "VP" [PL ("VBN","done")]],PL (".",".")]]
       )


-- | Reduced relative clause.
ex25 = ( "NASA enhances online scientific tool used by hundreds of scientists."
       , 5
       , (Present,Simple,Passive,[],Nothing)
       , [(0,("NASA","NASA")),(1,("enhance","enhances")),(2,("online","online")),(3,("scientific","scientific")),(4,("tool","tool")),(5,("use","used")),(6,("by","by")),(7,("hundred","hundreds")),(8,("of","of")),(9,("scientist","scientists")),(10,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","NASA")],PN "VP" [PL ("VBZ","enhances"),PN "NP" [PN "NP" [PL ("JJ","online"),PL ("JJ","scientific"),PL ("NN","tool")],PN "VP" [PL ("VBN","used"),PN "PP" [PL ("IN","by"),PN "NP" [PN "NP" [PL ("NNS","hundreds")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NNS","scientists")]]]]]]],PL (".",".")]]
       )



-- | infinitive
ex26 = ( "I have something to do now."
       , 4
       , (Present,Simple,Active,["to"],Nothing)
       , [(0,("I","I")),(1,("have","have")),(2,("something","something")),(3,("to","to")),(4,("do","do")),(5,("now","now")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "NP" [PL ("NN","something"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","do"),PN "ADVP" [PL ("RB","now")]]]]]],PL (".",".")]]
       )

ex27 = ( "I have something to have now."
       , 4
       , (Present,Simple,Active,["to"],Nothing)
       , [(0,("I","I")),(1,("have","have")),(2,("something","something")),(3,("to","to")),(4,("have","have")),(5,("now","now")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "NP" [PL ("NN","something"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","have"),PN "ADVP" [PL ("RB","now")]]]]]],PL (".",".")]]
       )


ex28 = ( "I have to write a book."
       , 3
       , (Present, Simple, Active, ["have", "to"], Nothing)
       , [(0,("I","I")),(1,("have","have")),(2,("to","to")),(3,("write","write")),(4,("a","a")),(5,("book","book")),(6,(".","."))]         
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBP","have"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","book")]]]]],PL (".",".")]]
       )


ex29 = ( "I ought to write a book."
       , 3
       , (Present, Simple, Active, ["ought", "to"], Nothing)
       , [(0,("I","I")),(1,("ought","ought")),(2,("to","to")),(3,("write","write")),(4,("a","a")),(5,("book","book")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("MD","ought"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","book")]]]]],PL (".",".")]]
       )

ex30 = ( "I used to write a book."
       , 3
       , (Present, Simple, Active, ["use", "to"], Nothing)
       , [(0,("I","I")),(1,("use","used")),(2,("to","to")),(3,("write","write")),(4,("a","a")),(5,("book","book")),(6,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","I")],PN "VP" [PL ("VBD","used"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","write"),PN "NP" [PL ("DT","a"),PL ("NN","book")]]]]],PL (".",".")]]
       )

 
ex31 = ( "They will have the votes to pass it as soon as next week."
       , 2
       , (Present, Simple, Active, ["will"], Nothing)
       , [(0,("they","They")),(1,("will","will")),(2,("have","have")),(3,("the","the")),(4,("vote","votes")),(5,("to","to")),(6,("pass","pass")),(7,("it","it")),(8,("as","as")),(9,("soon","soon")),(10,("as","as")),(11,("next","next")),(12,("week","week")),(13,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("PRP","They")],PN "VP" [PL ("MD","will"),PN "VP" [PL ("VB","have"),PN "S" [PN "NP" [PL ("DT","the"),PL ("NNS","votes")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","pass"),PN "NP" [PL ("PRP","it")],PN "ADVP" [PN "ADVP" [PL ("RB","as"),PL ("RB","soon")],PN "PP" [PL ("IN","as"),PN "NP-TMP" [PL ("JJ","next"),PL ("NN","week")]]]]]]]],PL (".",".")]]
       )


ex32 = ( "A former executive threw his client under the bus by using his knowledge to do his own trading first."
       , 14
       , (Present, Simple, Active, ["to"], Nothing)
       , [(0,("a","A")),(1,("former","former")),(2,("executive","executive")),(3,("throw","threw")),(4,("he","his")),(5,("client","client")),(6,("under","under")),(7,("the","the")),(8,("bus","bus")),(9,("by","by")),(10,("use","using")),(11,("he","his")),(12,("knowledge","knowledge")),(13,("to","to")),(14,("do","do")),(15,("he","his")),(16,("own","own")),(17,("trading","trading")),(18,("first","first")),(19,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("DT","A"),PL ("JJ","former"),PL ("NN","executive")],PN "VP" [PL ("VBD","threw"),PN "NP" [PL ("PRP$","his"),PL ("NN","client")],PN "PP" [PL ("IN","under"),PN "NP" [PL ("DT","the"),PL ("NN","bus")]],PN "PP" [PL ("IN","by"),PN "S" [PN "VP" [PL ("VBG","using"),PN "NP" [PL ("PRP$","his"),PL ("NN","knowledge"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","do"),PN "NP" [PL ("PRP$","his"),PL ("JJ","own"),PL ("NN","trading")],PN "ADVP" [PL ("RB","first")]]]]]]]]],PL (".",".")]]
       )


ex33 = ( "E-commerce is not going to eliminate the retailing sector of the country."
       , 5
       , (Present,Simple,Active, ["be","go","to"], Just "not")
       , [(0,("e-commerce","E-commerce")),(1,("be","is")),(2,("not","not")),(3,("go","going")),(4,("to","to")),(5,("eliminate","eliminate")),(6,("the","the")),(7,("retailing","retailing")),(8,("sector","sector")),(9,("of","of")),(10,("the","the")),(11,("country","country")),(12,(".","."))]
       , PN "ROOT" [PN "S" [PN "NP" [PL ("NN","E-commerce")],PN "VP" [PL ("VBZ","is"),PL ("RB","not"),PN "VP" [PL ("VBG","going"),PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","eliminate"),PN "NP" [PN "NP" [PL ("DT","the"),PL ("NN","retailing"),PL ("NN","sector")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("DT","the"),PL ("NN","country")]]]]]]]],PL (".",".")]]
       )

testcases :: [TestVerbProp]
testcases = [ ex1, ex2, ex3, ex4, ex5, ex6, ex7, ex8, ex9,ex10
            ,ex11,ex12,ex13,ex14,ex15,ex16,ex17,ex18,ex19,ex20
            ,ex21,ex22,ex23,ex24,ex25,ex26,ex27,ex28,ex29,ex30
            ,ex31,ex32,ex33
            ]




checkVP :: TestVerbProp -> Bool
checkVP (_txt,i,expresult,lmatknlst,pt) =
  let vps = mkVPS lmatknlst pt
  in case find (\vp -> vp^.vp_index == i) vps of
       Just vp -> expresult == (vp^.vp_tense,vp^.vp_aspect,vp^.vp_voice,vp^..vp_auxiliary.traverse._2._2.to unLemma,vp^?vp_negation._Just._2._2.to unLemma)
       _       -> False


formatTitle :: TestVerbProp -> String
formatTitle c = printf "- %-80s %4s%4s%4s: %-10s: %-5s"
                 (c^._1)
                 (formatTense  (c^._3._1))
                 (formatAspect (c^._3._2))
                 (formatVoice  (c^._3._3))
                 (T.intercalate " " (c^._3._4))
                 (fromMaybe "" (c^._3._5)) 


unitTests :: TestTree
unitTests =
  testGroup "verb property" . flip map testcases $ \c ->
    testCase (formatTitle c) $
      (checkVP c == True) @? (intercalate "\n" ((mkVPS (c^._4) (c^._5))^..traverse.to (formatVerbProperty fmtfunc))  ++ "\n" ++ T.unpack (prettyPrint 0 (c^._5)))


fmtfunc :: Zipper as -> Text
fmtfunc = either (const "") (tokenWord.snd) . getRoot . current



mainShow :: IO ()
mainShow = do
  flip mapM_ testcases $ \c -> do
    putStrLn "--------------------------------------------------------------------------"
    T.IO.putStrLn (c^._1)
    mapM_ (putStrLn.formatVerbProperty fmtfunc) (mkVPS (c^._4) (c^._5))
