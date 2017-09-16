{-# LANGUAGE DataKinds #-}

module NLP.Syntax.Preposition where

import           Control.Monad           (guard)
import           Data.List               (find)
import           Data.Maybe              (fromMaybe)
--
import           Data.BitreeZipper       (current,parent)
import           Data.Range              (Range)
import           NLP.Type.PennTreebankII (ChunkTag(..),getRange)
import           NLP.Type.TagPos         (TagPos(..),TokIdx(..),BeginEnd)
--
import           NLP.Syntax.Util         (isChunkAs)
import           NLP.Syntax.Type         (MarkType(..))
import           NLP.Syntax.Type.XBar    (Zipper,DPorPP(..))


beginEndToRange :: BeginEnd TokIdx -> Range
beginEndToRange (TokIdx b,TokIdx e) = (b,e-1)


checkEmptyPrep tagged z =
  let r = fromMaybe False $ do
            let rng = getRange (current z)
            -- check bare noun adverb
            find (\(TagPos (b,e,t)) -> beginEndToRange (b,e) == rng && t == MarkTime) tagged
            return (hasEmptyPreposition z)
  in if r then PrepP Nothing z else DP z




hasEmptyPreposition :: Zipper as -> Bool
hasEmptyPreposition z =
  fromMaybe False $ do
    guard (isChunkAs NP (current z))
    case parent z of
      Nothing -> return True
      Just z' -> do
        guard (not (isChunkAs PP (current z')) && not (isChunkAs ADVP (current z')))
        return True 


