{-# LANGUAGE DataKinds #-}

module NLP.Syntax.Preposition where

import           Control.Lens            ((^?),_Left,_1)
import           Control.Monad           (guard)
import           Data.Maybe              (fromMaybe)
import           Data.List               (find)
--
import           Data.Bitree             (getNodes,getRoot)
import           Data.BitreeZipper       (current,extractZipperById,mkBitreeZipper,parent)
import           Data.Range              (Range)
import           NLP.Type.PennTreebankII (ChunkTag(..),Lemma)
import           NLP.Type.TagPos         (TokIdx(..),BeginEnd)
--
import           NLP.Syntax.Util         (isChunkAs)
import           NLP.Syntax.Type.XBar    (BitreeICP,Zipper)


beginEndToRange :: BeginEnd TokIdx -> Range
beginEndToRange (TokIdx b,TokIdx e) = (b,e-1)


hasEmptyPreposition :: Zipper '[Lemma] -> Bool
hasEmptyPreposition z =
  fromMaybe False $ do
    guard (isChunkAs NP (current z))
    case parent z of
      Nothing -> return True
      Just z' -> do
        guard (not (isChunkAs PP (current z')) && not (isChunkAs ADVP (current z')))
        return True 


