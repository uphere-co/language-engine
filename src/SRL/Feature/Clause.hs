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




showClauseLevel ptree  = do
  -- T.IO.putStrLn $ T.intercalate " " (map snd (toList testpt2))
  let tr = clauseLevel (bimap N.convert id (getADTPennTree ptree))
      tr' = bimap f id tr
        where f (N.CL c,l) = T.pack (show c) <> ":" <> T.pack (show l)
              f (N.PH p,l) = T.pack (show p) <> ":" <> T.pack (show l)
              f (N.RT  ,l) = "ROOT" <> ":" <> T.pack (show l)
              -- g       = -- T.pack (show p)
      
  T.IO.putStrLn (formatBitree id tr')
  -- print (clauseForest tr)
