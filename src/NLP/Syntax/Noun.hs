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
import           NLP.Syntax.Type.XBar     (Zipper,SplitType(..),DetP,XP(..),maximalProjection)
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs)


mkOrdDP :: Zipper a -> DetP a
mkOrdDP z = XP (rf z,rf z) z () Nothing Nothing
  where rf = getRange . current 


mkSplittedDP :: SplitType -> Range -> Range -> Zipper a -> DetP a
mkSplittedDP typ h m o = case typ of
                           CLMod -> XP (rf o,h) o () Nothing  (Just m)
                           BNMod -> XP (rf o,h) o () (Just m) Nothing
                           APMod -> XP (rf o,h) o () (Just m) Nothing  -- apposition is an adjunct.
  where rf = getRange . current 


splitDP :: [TagPos TokIdx MarkType]
        -> Zipper (Lemma ': as)
        -> DetP (Lemma ': as)
splitDP tagged z = bareNounModifier tagged . fromMaybe (mkOrdDP z) $ do
  guard (isChunkAs NP (current z))
  dp <- child1 z
  guard (isChunkAs NP (current dp))
  sbar <- next dp
  let rf = getRange . current
  ((guard (isChunkAs SBAR (current sbar)) >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
   (guard (isChunkAs VP (current sbar))   >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)))


-- | This function is very ad hoc. Later we should have PP according to X-bar theory
--
splitPP :: [TagPos TokIdx MarkType] -> Zipper (Lemma ': as) -> DetP (Lemma ': as)
splitPP tagged z = fromMaybe (mkOrdDP z) $ do
  guard (isChunkAs PP (current z))
  p <- child1 z
  guard (isPOSAs TO (current p) || isPOSAs IN (current p))
  dp <- next p
  return (splitDP tagged dp)


-- | Identify bare noun subexpression inside noun phrase as modifier.
--   I did not implement the already-splitted case. We need multiple-adjunct
--   structure. 
--
bareNounModifier :: [TagPos TokIdx MarkType]
                 -> DetP (Lemma ': as)
                 -> DetP (Lemma ': as)
bareNounModifier tagged x = fromMaybe x $ do
  let z = x^.maximalProjection
  guard (isChunkAs NP (current z))
  let rng@(b0,e0) = getRange (current z)
  -- check entity for the last words
  let f (b0,e0) (b1,e1) = e0 == e1 && b0 < b1
  TagPos (b1'',e1'',t) <- find (\(TagPos (b1',e1',t)) -> f rng (beginEndToRange (b1',e1')) && t == MarkEntity) tagged
  let (b1,e1) = beginEndToRange (b1'',e1'')
      idx_last_modifier_word = b1-1
  last_modifier_word <- find (\x -> x^._1 == idx_last_modifier_word) (toList (current z))
  guard (last_modifier_word^._2.to posTag.to isNoun == Yes)
  return (mkSplittedDP BNMod (b1,e1) (b0,idx_last_modifier_word) z)
