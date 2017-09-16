{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Monad            (guard)
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
--
import           Data.BitreeZipper        (child1,current,next)
import           NLP.Type.PennTreebankII  (ChunkTag(..),Lemma,POSTag(..)
                                          ,getRange)
import           NLP.Type.TagPos          (TagPos(..),TokIdx)
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper)
import           NLP.Syntax.Util          (isChunkAs, isPOSAs)


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




splitOutModifierDP :: [TagPos TokIdx MarkType]
                   -> Zipper (Lemma ': as)
                   -> Maybe (Zipper (Lemma ': as),Zipper (Lemma ': as))
splitOutModifierDP tagged z = Nothing
