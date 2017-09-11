{-# LANGUAGE DataKinds #-}

module NLP.Syntax.Preposition where

import           Control.Monad           (guard)
import           Data.Maybe              (fromMaybe)

--

import           Data.BitreeZipper       (current,parent)
import           Data.Range              (Range)
import           NLP.Type.PennTreebankII (ChunkTag(..))
import           NLP.Type.TagPos         (TokIdx(..),BeginEnd)
--
import           NLP.Syntax.Util         (isChunkAs)
import           NLP.Syntax.Type.XBar    (Zipper)


beginEndToRange :: BeginEnd TokIdx -> Range
beginEndToRange (TokIdx b,TokIdx e) = (b,e-1)


hasEmptyPreposition :: Zipper as -> Bool
hasEmptyPreposition z =
  fromMaybe False $ do
    guard (isChunkAs NP (current z))
    case parent z of
      Nothing -> return True
      Just z' -> do
        guard (not (isChunkAs PP (current z')) && not (isChunkAs ADVP (current z')))
        return True 


