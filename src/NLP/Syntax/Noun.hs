{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Monad            (guard)
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
--
import           Data.BitreeZipper        (child1,current,next)
import           Data.Range               (Range)
import           NLP.Type.PennTreebankII  (ChunkTag(..),Lemma,POSTag(..)
                                          ,getRange)
import           NLP.Type.TagPos          (TagPos(..),TokIdx)
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper)
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs)


splitDP :: Zipper (Lemma ': as) -> Maybe (Zipper (Lemma ': as))
splitDP z = do
  guard (isChunkAs NP (current z))
  dp <- child1 z
  guard (isChunkAs NP (current dp))
  sbar <- next dp
  ((guard (isChunkAs SBAR (current sbar)) >> return dp) <|>
   (guard (isChunkAs VP (current sbar)) >> return dp))


-- | this function is very ad hoc. Later we should have PP according to X-bar theory
splitPP :: Zipper (Lemma ': as) -> Maybe (Zipper (Lemma ': as))
splitPP z = do
  guard (isChunkAs PP (current z))
  p <- child1 z
  guard (isPOSAs TO (current p) || isPOSAs IN (current p))
  dp <- next p
  return (fromMaybe dp (splitDP dp))




bareNounModifier :: [TagPos TokIdx MarkType]
                 -> Zipper (Lemma ': as)
                 -> Maybe (Range,Range)
bareNounModifier tagged z = do
  guard (isChunkAs NP (current z))
  let rng@(b0,e0) = getRange (current z)
  -- check entity for the last words
  let f (b0,e0) (b1,e1) = e0 == e1 && b0 < b1
  TagPos (b1'',e1'',t) <- find (\(TagPos (b1',e1',t)) -> f rng (beginEndToRange (b1',e1')) && t == MarkEntity) tagged
  let (b1,e1) = beginEndToRange (b1'',e1'')

  return ((b0,b1-1),(b1,e1))
