{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Tasty.HUnit
import           Test.Tasty
--
import           Data.Bitree              (getRoot)
import           NLP.Type.PennTreebankII
--
import           PropBank.Match


ex1 :: PennTree
ex1 = PN "ROOT" [PN "S" [PN "S-TPC-1" [PN "NP-SBJ" [PN "NP" [PN "NP" [PL ("DT","A"),PL ("NN","form")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NN","asbestos")]]],PN "VP" [PN "ADVP-TMP" [PL ("RB","once")],PL ("VBN","used"),PN "NP" [PL ("-NONE-","*")],PN "S-CLR" [PN "NP-SBJ" [PL ("-NONE-","*PRO*")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","make"),PN "NP" [PL ("NNP","Kent"),PL ("NN","cigarette"),PL ("NNS","filters")]]]]]],PN "VP" [PL ("VBZ","has"),PN "VP" [PL ("VBN","caused"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","high"),PL ("NN","percentage")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("NN","cancer"),PL ("NNS","deaths")]],PN "PP-LOC" [PL ("IN","among"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","group")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("NNS","workers")],PN "VP" [PL ("VBN","exposed"),PN "NP" [PL ("-NONE-","*")],PN "PP-CLR" [PL ("IN","to"),PN "NP" [PL ("PRP","it")]],PN "ADVP-TMP" [PN "NP" [PN "QP" [PL ("RBR","more"),PL ("IN","than"),PL ("CD","30")],PL ("NNS","years")],PL ("IN","ago")]]]]]]]]]],PL (",",","),PN "NP-SBJ" [PL ("NNS","researchers")],PN "VP" [PL ("VBD","reported"),PN "SBAR" [PL ("-NONE-","0"),PN "S" [PL ("-NONE-","*T*-1")]]],PL (".",".")]]


main :: IO ()
main = do -- defaultMain unitTests
  print $ map (adjustIndexFromTree ex1) [0..30]
  
check1 = getRoot ex1 == Left "ROOT"


unitTests :: TestTree
unitTests = testGroup "adjust index"
              [ testCase "trivial"  (check1 @?= True) ]
