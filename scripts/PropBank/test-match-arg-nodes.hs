{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Foldable
import           Data.Maybe 
import           Data.Monoid
import           Data.Text                (Text)
import           Test.Tasty.HUnit
import           Test.Tasty
--
-- import           Data.Bitree              (getRoot)
import           NLP.Type.PennTreebankII
--
-- import           PropBank.Format
import           PropBank.Match
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Type.Match


proptxt :: Text
proptxt = "nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 14 gold say-v say.01 ----- 14:0-rel 0:2-ARG0 15:1-ARG1\n\
          \nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 17 gold expect-v expect.01 ----- 17:0-rel 16:1-ARG0 18:2-ARG1\n\
          \nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 24 gold be-v be.01 ----- 24:0-rel 18:1-ARG1 25:2-ARG2\n\
          \nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 35 gold be-v be.01 ----- 35:0-rel 34:0-ARG1 36:3-ARG2 30:1*34:0-LINK-SLC\n"


coretr :: PennTree
coretr = PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("NNP","Kysor")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","maker")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("JJ","heavy-duty"),PL ("NN","truck")],PL ("CC","and"),PN "NP" [PL ("JJ","commercial"),PL ("NN","refrigeration"),PL ("NN","equipment")]]]],PL (",",",")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","expects"),PN "NP" [PL ("PRP$","its"),PL ("JJ","fourth-quarter"),PL ("NNS","earnings")],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","be"),PN "ADJP" [PL ("RBR","more"),PL ("RB","closely")],PN "PP" [PL ("IN","in"),PN "NP" [PL ("NN","line")]],PN "PP" [PL ("IN","with"),PN "NP" [PN "NP" [PL ("JJ","usual"),PL ("NNS","levels")],PL (",",","),PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "VP" [PL ("VBP","are"),PN "PP" [PL ("IN","between"),PN "NP" [PN "NP" [PL ("CD","30"),PL ("NNS","cents")],PL ("CC","and"),PN "NP" [PN "NP" [PL ("CD","50"),PL ("NNS","cents")],PN "NP" [PL ("DT","a"),PL ("NN","share")]]]]]]]]]]]]]]]],PL (".",".")]]


proptr :: PennTree
proptr = PN "ROOT" [PN "S" [PN "NP-SBJ" [PN "NP" [PL ("NNP","Kysor")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","maker")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NML" [PL ("JJ","heavy"),PL ("HYPH","-"),PL ("NN","duty")],PN "NML" [PN "NML" [PL ("NN","truck")],PL ("CC","and"),PN "NML" [PL ("JJ","commercial"),PL ("NN","refrigeration")]],PL ("NN","equipment")]]],PL (",",",")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PL ("-NONE-","0"),PN "S" [PN "NP-SBJ" [PL ("PRP","it")],PN "VP" [PL ("VBZ","expects"),PN "S" [PN "NP-SBJ" [PL ("PRP$","its"),PN "NML" [PL ("JJ","fourth"),PL ("HYPH","-"),PL ("NN","quarter")],PL ("NNS","earnings")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","be"),PN "PP-PRD" [PN "ADVP" [PL ("RBR","more"),PL ("RB","closely")],PL ("IN","in"),PN "NP" [PN "NP" [PL ("NN","line")],PN "PP" [PL ("IN","with"),PN "NP" [PN "NP" [PL ("JJ","usual"),PL ("NNS","levels")],PL (",",","),PN "SBAR" [PN "WHNP-1" [PL ("WDT","which")],PN "S" [PN "NP-SBJ" [PL ("-NONE-","*T*-1")],PN "VP" [PL ("VBP","are"),PN "NP-PRD" [PN "NP" [PN "QP" [PL ("IN","between"),PL ("CD","30"),PL ("NNS","cents"),PL ("CC","and"),PL ("CD","50"),PL ("NNS","cents")],PL ("-NONE-","*U*")],PN "NP-ADV" [PL ("DT","a"),PL ("NN","share")]]]]]]]]]]]]]]]],PL (".",".")]]


testex1 :: [(Int,[(String,[Range])])]
testex1 = [(14,[("ARG0",[(12,12)])])]


main' :: IO ()
main' = do
  print (toList (mkPennTreeIdx proptr))
  let insts = parsePropWithFileField NoOmit proptxt

  print insts
  
  flip mapM_ insts $ \inst -> do
    putStrLn "=="
    let args = inst^.inst_arguments
    flip mapM_ args $ \arg -> do
      putStrLn "--"
      print (matchArgNodes (coretr,proptr) arg)


testPredArg :: (String -> IO ()) -> (Int,(Text,Text),Int,Range) -> Assertion
testPredArg step (i,(lma,rolnum),n,rng) = do
  step ("finding lemma: index " ++ show i)
  let insts = parsePropWithFileField NoOmit proptxt
      minst = find (\inst->inst^.inst_predicate_id == i) insts
  assertBool "not found instance" (isJust minst)
  let Just inst = minst
  let rolesettxt = show (lma <> "." <> rolnum)
  assertBool ("not same as " ++ rolesettxt ) $ inst^.inst_lemma_roleset_id == (lma,rolnum)
  step ("found roleset : " ++ rolesettxt)
  step ("finding arg" ++ show n)
  let marg = find (\arg->arg^.arg_label == NumberedArgument n) (inst^.inst_arguments)
  assertBool "not found argument" (isJust marg)
  let Just arg = marg
      argnodes = matchArgNodes (coretr,proptr) arg
  assertBool "null argument nodes" (not (null argnodes))
  step ("finding first node")
  let argnode = argnodes !! 0
  assertBool ("first argument node is not matched with " ++ show rng) (argnode^.mn_node._1 == rng)


unitTests :: TestTree
unitTests = testGroup "match instance"
              [ testCaseSteps "match instance" $ \step -> do
                  testPredArg step (14,("say","01"),0,(0,11))
                  testPredArg step (35,("be","01"),1,(28,28))
              ]


main :: IO ()
main = defaultMain unitTests

