{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module NLP.Syntax.Preposition where

import           Control.Lens            ((^.),(.~))
import           Control.Monad           (guard)
import           Data.List               (find)
import           Data.Maybe              (fromMaybe)
--
import           Data.BitreeZipper       (current,parent)

import           NLP.Type.PennTreebankII (ChunkTag(..),getRange)
import           NLP.Type.TagPos         (TagPos(..),TokIdx)
--
import           NLP.Syntax.Util         (beginEndToRange,isChunkAs)
import           NLP.Syntax.Type         (MarkType(..))
import           NLP.Syntax.Type.XBar    (Zipper,DetP,CompVP(..)
                                         ,Prep(..),PrepClass(..),PP,XP(..)
                                         ,complement,headX,maximalProjection
                                         ,original,mkPP
                                         )


hasEmptyPreposition :: Zipper as -> Bool
hasEmptyPreposition z =
  fromMaybe False $ do
    guard (isChunkAs NP (current z))
    case parent z of
      Nothing -> return True
      Just z' -> do
        guard (not (isChunkAs PP (current z')) && not (isChunkAs ADVP (current z')))
        return True


isMatchedTime rng (TagPos (b,e,t)) = beginEndToRange (b,e) == rng && t == MarkTime



checkEmptyPrep :: [TagPos TokIdx MarkType] -> DetP t -> CompVP t
checkEmptyPrep tagged dp =
  let z = dp^.maximalProjection.original
      r = fromMaybe False $ do
            let rng = getRange (current z)
            -- check bare noun adverb
            find (isMatchedTime rng) tagged
            return (hasEmptyPreposition z)
  in if r
     then CompVP_PP (mkPP (Prep_NULL,PC_Time) z dp)
     else CompVP_DP dp


checkTimePrep :: [TagPos TokIdx MarkType] -> PP t -> CompVP t
checkTimePrep tagged pp =
  let dp = pp^.complement
      z = dp^.maximalProjection.original
      (prep,pclass) = pp^.headX
      r = fromMaybe False $ do
            let rng = getRange (current z)
            find (isMatchedTime rng) tagged
            return True
  in if r then CompVP_PP ((headX .~ (prep,PC_Time)) pp) else CompVP_PP pp
