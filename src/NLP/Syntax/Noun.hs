{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.),to,_1,_2)
import           Control.Monad            (guard)
import           Data.Foldable            (toList)
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
--
import           Data.BitreeZipper        (child1,current,next)
import           Data.Range               (Range)
import           NLP.Type.PennTreebankII  (ChunkTag(..),Lemma,POSTag(..),TernaryLogic(..)
                                          ,getRange,isNoun,posTag)
import           NLP.Type.TagPos          (TagPos(..),TokIdx)
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper,SplitDP(..),SplittedDP(..),SplitType(..))
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs)


mkSplittedDP :: SplitType -> Zipper a -> Zipper a -> Zipper a -> SplittedDP (Zipper a)
mkSplittedDP typ h m o = SplittedDP typ (rf h) (rf m) o
  where rf = getRange . current 


splitDP :: Zipper (Lemma ': as) -> SplitDP (Zipper (Lemma ': as))
splitDP z = fromMaybe (Unsplitted z) $ do
  guard (isChunkAs NP (current z))
  dp <- child1 z
  guard (isChunkAs NP (current dp))
  sbar <- next dp
  ((guard (isChunkAs SBAR (current sbar)) >> return (Splitted (mkSplittedDP CLMod dp sbar z))) <|>
   (guard (isChunkAs VP (current sbar)) >> return (Splitted (mkSplittedDP CLMod dp sbar z))))


-- | This function is very ad hoc. Later we should have PP according to X-bar theory
--
splitPP :: Zipper (Lemma ': as) -> SplitDP (Zipper (Lemma ': as))
splitPP z = fromMaybe (Unsplitted z) $ do
  guard (isChunkAs PP (current z))
  p <- child1 z
  guard (isPOSAs TO (current p) || isPOSAs IN (current p))
  dp <- next p
  return (splitDP dp)


-- | Identify bare noun subexpression inside noun phrase as modifier.
--   I did not implement the already-splitted case. We need multiple-adjunct
--   structure. 
--
bareNounModifier :: [TagPos TokIdx MarkType]
                 -> SplitDP (Zipper (Lemma ': as))
                 -> SplitDP (Zipper (Lemma ': as))
bareNounModifier _      x@(Splitted   _) = x                 -- for the time being
bareNounModifier tagged x@(Unsplitted z) = fromMaybe x $ do
  guard (isChunkAs NP (current z))
  let rng@(b0,e0) = getRange (current z)
  -- check entity for the last words
  let f (b0,e0) (b1,e1) = e0 == e1 && b0 < b1
  TagPos (b1'',e1'',t) <- find (\(TagPos (b1',e1',t)) -> f rng (beginEndToRange (b1',e1')) && t == MarkEntity) tagged
  let (b1,e1) = beginEndToRange (b1'',e1'')
      idx_last_modifier_word = b1-1
  last_modifier_word <- find (\x -> x^._1 == idx_last_modifier_word) (toList (current z))
  guard (last_modifier_word^._2.to posTag.to isNoun == Yes)
  return (Splitted (SplittedDP BNMod (b1,e1) (b0,idx_last_modifier_word) z))
