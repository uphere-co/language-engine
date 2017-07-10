{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable
import           Data.Monoid
import           Test.Tasty.HUnit
import           Test.Tasty
--
import           Data.Bitree              (getRoot)
import           NLP.Type.PennTreebankII
--
import           PropBank.Match


ex1 :: PennTree
ex1 = PN "ROOT" [PN "S" [PN "S-TPC-1" [PN "NP-SBJ" [PN "NP" [PN "NP" [PL ("DT","A"),PL ("NN","form")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NN","asbestos")]]],PN "VP" [PN "ADVP-TMP" [PL ("RB","once")],PL ("VBN","used"),PN "NP" [PL ("-NONE-","*")],PN "S-CLR" [PN "NP-SBJ" [PL ("-NONE-","*PRO*")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","make"),PN "NP" [PL ("NNP","Kent"),PL ("NN","cigarette"),PL ("NNS","filters")]]]]]],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","caused"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","high"),PL ("NN","percentage")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NN","cancer"),PL ("NNS","deaths")]],PN "PP-LOC" [PL ("IN","among"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","group")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("NNS","workers")],PN "VP" [PL ("VBN","exposed"),PN "NP" [PL ("-NONE-","*")],PN "PP-CLR" [PL ("IN","to"),PN "NP" [PL ("PRP","it")]],PN "ADVP-TMP" [PN "NP" [PN "QP" [PL ("RBR","more"),PL ("IN","than"),PL ("CD","30")],PL ("NNS","years")],PL ("IN","ago")]]]]]]]]]],PL (",",","),PN "NP-SBJ" [PL ("NNS","researchers")],PN "VP" [PL ("VBD","reported"),PN "SBAR" [PL ("-NONE-","0"),PN "S" [PL ("-NONE-","*T*-1")]]],PL (".",".")]]


testex1 = [(5,Right 5),(6,Left 6),(7,Left 6),(8,Right 6),(15,Right 13)]

ex2 :: PennTree
ex2 = PN "ROOT" [PN "SINV" [PL ("``","``"),PN "S-TPC-1" [PN "NP-SBJ" [PL ("EX","There")],PN "VP" [PL ("VBZ","'s"),PN "NP-PRD" [PN "NP" [PL ("DT","no"),PL ("NN","question")],PN "SBAR" [PL ("IN","that"),PN "S" [PN "NP-SBJ" [PN "NP" [PL ("DT","some")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("DT","those"),PL ("NNS","workers"),PL ("CC","and"),PL ("NNS","managers")]]],PN "VP" [PL ("VBD","contracted"),PN "NP" [PN "ADJP" [PL ("NN","asbestos"),PL ("HYPH","-"),PL ("VBN","related")],PL ("NNS","diseases")]]]]]]],PL (",",","),PL ("''","''"),PN "VP" [PL ("VBD","said"),PN "S" [PL ("-NONE-","*T*-1")]],PN "NP-SBJ" [PN "NP" [PL ("NNP","Darrell"),PL ("NNP","Phillips")],PL (",",","),PN "NP" [PN "NP" [PL ("NN","vice"),PL ("NN","president")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","human"),PL ("NNS","resources")]],PN "PP" [PL ("IN","for"),PN "NP" [PL ("NNP","Hollingsworth"),PL ("CC","&"),PL ("NNP","Vose")]]]],PL (".",".")]]

testex2 = [(11,Right 11),(13,Right 13),(14,Left 14),(15,Left 14),(19,Right 17),(20,Left 18),(21,Right 18)]

main' :: IO ()
main' = do
 mapM_ print (zip [0..] (toList ex1 ))  
 print (exclusionList ex1)
 putStrLn "==============================="
 mapM_ print (zip [0..] (toList ex2 ))
 print (getMerged ex2)
 print (exclusionList ex2)

main = defaultMain unitTests
   
check ex testex =  getAll (foldMap (\(i,ei') -> All (adjustIndexFromTree ex i == ei')) testex)


unitTests :: TestTree
unitTests = testGroup "adjust index"
              [ testCase "ex1" (check ex1 testex1 @?= True)
              , testCase "ex2" (check ex2 testex2 @?= True)
              ]
