{-# LANGUAGE OverloadedStrings #-}

module SRL.Feature.Clause where

import           Data.Bifunctor
import           Data.Foldable
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
--
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
--
import           SRL.Format


testpt1 :: PennTree
testpt1 = PN "ROOT" [PN "S" [PN "NP" [PL ("DT","The"),PL ("NN","man")],PN "PRN" [PL (",",","),PN "S" [PN "NP" [PL ("PRP","it")],PN "VP" [PL ("VBZ","seems")]],PL (",",",")],PN "VP" [PL ("VBZ","has"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","Lichtenstein"),PL ("NN","corporation")],PL (",",","),PN "VP" [PL ("VBN","licensed"),PN "PP" [PL ("IN","in"),PN "NP" [PN "NP" [PL ("NNP","Libya")],PL ("CC","and"),PN "NP" [PL ("JJ","sheltered")]]],PN "PP" [PL ("IN","in"),PN "NP" [PL ("DT","the"),PL ("NNPS","Bahamas")]]]]],PL (".",".")]]

testpt2 :: PennTree
testpt2 = PN "ROOT" [PN "S" [PN "NP" [PL ("NNP","President"),PL ("NNP","Donald"),PL ("NNP","Trump")],PN "VP" [PL ("VBD","said"),PN "SBAR" [PN "S" [PN "NP" [PL ("PRP","he")],PN "VP" [PL ("VBZ","'s"),PN "VP" [PN "ADVP" [PL ("RB","actively")],PN "VP" [PL ("VBG","considering"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("NN","breakup")],PN "PP" [PL ("IN","of"),PN "NP" [PL ("JJ","giant"),PL ("NNP","Wall"),PL ("NNP","Street"),PL ("NNS","banks")]]]],PL (",",","),PN "VP" [PL ("VBG","giving"),PN "NP" [PL ("DT","a"),PL ("NN","push")],PN "PP" [PL ("TO","to"),PN "NP" [PL ("NNS","efforts")]],PN "S" [PN "VP" [PL ("TO","to"),PN "VP" [PL ("VB","revive"),PN "NP" [PN "NP" [PL ("DT","a"),PL ("JJ","Depression-era"),PL ("NN","law")],PN "VP" [PL ("VBG","separating"),PN "NP" [PL ("NN","consumer"),PL ("CC","and"),PL ("NN","investment"),PL ("NN","banking")]]]]]]]]]]]],PL (".",".")]]

{- 
clauseForest :: Bitree N.CombinedTag (POSTag,Text) -> [Bitree N.ClauseTag (Either POSTag N.PhraseTag)]
clauseForest (PN N.RT xs) = concatMap clauseForest xs
clauseForest (PN (N.CL c) xs) = [PN c (concatMap clauseForest xs)]
clauseForest (PN (N.PH p) xs) = [PL (Right p)]
clauseForest (PL (pos,_))   = [PL (Left pos)]


-}

currentlevel (PN (_,l) _) = l
currentlevel (PL _ )      = 0


clauseLevel :: Bitree N.CombinedTag (POSTag,Text) -> Bitree (N.CombinedTag,Int) Text
clauseLevel (PN tag xs) = let ys = map clauseLevel xs
                              lvl = maximum (map currentlevel ys)
                          in case tag of
                               N.CL _ -> PN (tag,lvl+1) ys
                               N.PH p -> if lvl == 0 && p /= N.VP
                                         then PL (T.pack (show p))
                                         else PN (tag,lvl  ) ys
                               N.RT   -> PN (N.RT,lvl) ys
clauseLevel (PL pt)     = PL (T.pack (show pt))




main = do
  T.IO.putStrLn $ T.intercalate " " (map snd (toList testpt2))
  let tr = clauseLevel (bimap N.convert id (getADTPennTree testpt2))

      tr' = bimap f id tr
        where f (N.CL c,l) = T.pack (show c) <> ":" <> T.pack (show l)
              f (N.PH p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (N.RT  ,l) = "ROOT" <> ":" <> T.pack (show l)
              -- g       = -- T.pack (show p)
      
  T.IO.putStrLn (formatBitree id tr')
  -- print (clauseForest tr)
