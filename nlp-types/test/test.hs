{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text 
import           Data.Text                   (Text)
import qualified Data.Text.IO         as TIO
--
import           NLP.Parser.PennTreebankII

main :: IO ()
main = do
  let txt = "( (NP-VOC-UNF (JJ Dear) (. ,)))"
      txt2 = 
        "( (S (ADVP-TMP (NP (DT A) (JJ few) (NNS months))\n\
        \               (RB ago))\n\
        \     (NP-SBJ (PRP you))\n\
        \     (VP (VBD received)\n\
        \        (NP (NP (NP (DT a) (NN letter))\n\
        \                (PP (IN from)\n\
        \                    (NP (PRP me))))\n\
        \            (VP (VBG telling)\n\
        \                (NP (NP (DT the) (NN success) (NNS stories))\n\
        \                    (PP (IN of)\n\
        \                        (NP (NP (NNS people))\n\
        \                            (SBAR (WHNP-1 (WP who))\n\
        \                                  (S (NP-SBJ (-NONE- *T*-1))\n\
        \                                     (VP (VBD got)\n\
        \                                         (NP (NNS jobs))\n\
        \                                         (PP-MNR (IN with)\n\
        \                                                 (NP (NP (NNP Goodwill) (POS 's))\n\
        \                                                     (NN help))))))))))))\n\
        \     (. .)))"

  print (parseOnly penntree txt)
  print (parseOnly penntree txt2)
