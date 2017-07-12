{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Foldable
import           Data.Monoid
import           Test.Tasty.HUnit
import           Test.Tasty
--
import           Data.Bitree              (getRoot)
import           NLP.Type.PennTreebankII
--
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Type.Match
import           PropBank.Match

proptxt = "nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 14 gold say-v say.01 ----- 14:0-rel 0:2-ARG0 15:1-ARG1\n\
          \nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 17 gold expect-v expect.01 ----- 17:0-rel 16:1-ARG0 18:2-ARG1\n\
          \nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 24 gold be-v be.01 ----- 24:0-rel 18:1-ARG1 25:2-ARG2\n\
          \nw/wsj/24/wsj_2445@2445@wsj@nw@en@on 5 35 gold be-v be.01 ----- 35:0-rel 34:0-ARG1 36:3-ARG2 30:1*34:0-LINK-SLC\n"

coretr = PN "ROOT" [PN "S" [PN "NP" [PN "NP" [PL ("NNP","Kysor")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","maker")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NP" [PL ("JJ","heavy-duty"),PL ("NN","truck")],PL ("CC","and"),PN "NP" [PL ("JJ","commercial"),PL ("NN","refrigeration"),PL ("NN","equipment")]]]],PL (",",",")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","expects"),PN "NP" [PL ("PRP$","its"),PL ("JJ","fourth-quarter"),PL ("NNS","earnings")],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","be"),PN "ADJP" [PL ("RBR","more"),PL ("RB","closely")],PN "PP" [PL ("IN","in"),PN "NP" [PL ("NN","line")]],PN "PP" [PL ("IN","with"),PN "NP" [PN "NP" [PL ("JJ","usual"),PL ("NNS","levels")],PL (",",","),PN "SBAR" [PN "WHNP" [PL ("WDT","which")],PN "S" [PN "VP" [PL ("VBP","are"),PN "PP" [PL ("IN","between"),PN "NP" [PN "NP" [PL ("CD","30"),PL ("NNS","cents")],PL ("CC","and"),PN "NP" [PN "NP" [PL ("CD","50"),PL ("NNS","cents")],PN "NP" [PL ("DT","a"),PL ("NN","share")]]]]]]]]]]]]]]]],PL (".",".")]]


proptr = PN "ROOT" [PN "S" [PN "NP-SBJ" [PN "NP" [PL ("NNP","Kysor")],PL (",",","),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","maker")],PN "PP" [PL ("IN","of"),PN "NP" [PN "NML" [PL ("JJ","heavy"),PL ("HYPH","-"),PL ("NN","duty")],PN "NML" [PN "NML" [PL ("NN","truck")],PL ("CC","and"),PN "NML" [PL ("JJ","commercial"),PL ("NN","refrigeration")]],PL ("NN","equipment")]]],PL (",",",")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PL ("-NONE-","0"),PN "S" [PN "NP-SBJ" [PL ("PRP","it")],PN "VP" [PL ("VBZ","expects"),PN "S" [PN "NP-SBJ" [PL ("PRP$","its"),PN "NML" [PL ("JJ","fourth"),PL ("HYPH","-"),PL ("NN","quarter")],PL ("NNS","earnings")],PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","be"),PN "PP-PRD" [PN "ADVP" [PL ("RBR","more"),PL ("RB","closely")],PL ("IN","in"),PN "NP" [PN "NP" [PL ("NN","line")],PN "PP" [PL ("IN","with"),PN "NP" [PN "NP" [PL ("JJ","usual"),PL ("NNS","levels")],PL (",",","),PN "SBAR" [PN "WHNP-1" [PL ("WDT","which")],PN "S" [PN "NP-SBJ" [PL ("-NONE-","*T*-1")],PN "VP" [PL ("VBP","are"),PN "NP-PRD" [PN "NP" [PN "QP" [PL ("IN","between"),PL ("CD","30"),PL ("NNS","cents"),PL ("CC","and"),PL ("CD","50"),PL ("NNS","cents")],PL ("-NONE-","*U*")],PN "NP-ADV" [PL ("DT","a"),PL ("NN","share")]]]]]]]]]]]]]]]],PL (".",".")]]


main = do
  let insts = parsePropWithFileField NoOmit proptxt

  let minsts = matchInstances (coretr,proptr) insts

  -- print minsts

  flip mapM_ minsts $ \minst-> do
    let inst = minst^.mi_instance
        args = minst^.mi_arguments
    print (findRelNode (minst^.mi_arguments),inst^.inst_lemma_roleset_id)
    mapM_ (print . (\a->(a^.ma_argument.arg_label.to pbLabelText,a^..ma_nodes.traverse.mn_node._1))) args
  
{-     
  testlst = (5,(coretr,proptr)), )]
  
  void . runEitherT $ do
    lst' <- loadMatchArticle ptreedir framedir basedir article
    let lst = concat lst'
    liftIO . flip mapM_ lst $ \(i,(((coretr,coredep,corelma),proptr),insts)) -> do
      putStrLn "-------------"
      putStrLn $ "sentence " ++ show i
      let tokens = map (^._2) . toList $ coretr
      T.IO.putStrLn (T.intercalate " " tokens)
      putStrLn "--"
      putStrLn $ "propbank"
      let minsts = matchInstances (coretr,proptr) insts
      flip mapM_ minsts $ \minst-> do
        let inst = minst^.mi_instance
            args = minst^.mi_arguments
        print (findRelNode (minst^.mi_arguments),inst^.inst_lemma_roleset_id)
        mapM_ (print . (\a->(a^.ma_argument.arg_label.to pbLabelText,a^..ma_nodes.traverse.mn_node._1))) args
   -}  
           
