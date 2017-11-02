{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Preposition where

import           Control.Lens            ((^.),(.~),(&))
import           Control.Monad           (guard)
import           Data.List               (find)
import           Data.Maybe              (fromMaybe)
--
import           Data.BitreeZipper       (current,parent,extractZipperByRange)
import           Data.Range              (isInsideR,Range)
import           NLP.Type.PennTreebankII (ChunkTag(..),getRange)
import           NLP.Type.TagPos         (TagPos(..),TokIdx)
--
import           NLP.Syntax.Util         (beginEndToRange,isChunkAs)
import           NLP.Syntax.Type         (MarkType(..))
import           NLP.Syntax.Type.XBar    (Zipper,DetP,CompVP(..)
                                         ,Prep(..),PrepClass(..),PP
                                         ,TaggedLemma(..),pennTree,tagList
                                         ,complement,headX,maximalProjection
                                         ,mkPP)


hasEmptyPreposition :: TaggedLemma t -> Range -> Bool
hasEmptyPreposition tagged rng =
  fromMaybe False $ do
    z <- extractZipperByRange rng (tagged^.pennTree)
    guard (isChunkAs NP (current z))
    case parent z of
      Nothing -> return True
      Just z' -> do
        guard (not (isChunkAs PP (current z')) && not (isChunkAs ADVP (current z')))
        return True


isMatchedTime :: Range -> TagPos TokIdx MarkType -> Bool
isMatchedTime rng (TagPos (b,e,t)) = beginEndToRange (b,e) == rng && t == MarkTime


checkEmptyPrep :: TaggedLemma t -> DetP t -> CompVP t
checkEmptyPrep tagged dp =
  let rng = dp^.maximalProjection
      r = fromMaybe False $ do
            -- check bare noun adverb
            find (isMatchedTime rng) (tagged^.tagList)
            return (hasEmptyPreposition tagged rng)
  in if r
     then CompVP_PP (mkPP (Prep_NULL,PC_Time) rng dp)
     else CompVP_DP dp


checkTimePrep :: TaggedLemma t -> PP t -> CompVP t
checkTimePrep tagged pp =
  let dp = pp^.complement
      (prep,_pclass) = pp^.headX
      r = fromMaybe False $ do
            let rng = dp^.maximalProjection
            find (isMatchedTime rng) (tagged^.tagList)
            return True
  in if r then CompVP_PP ((headX .~ (prep,PC_Time)) pp) else CompVP_PP pp



identifyInternalTimePrep :: TaggedLemma t
                         -> DetP t
                         -> (DetP t,[Zipper t])
identifyInternalTimePrep tagged dp = fromMaybe (dp,[]) $ do
  let rng_dp@(b_dp,_e_dp) = dp^.maximalProjection
  TagPos (b0,e0,_)
    <- find (\(TagPos (b,e,t)) -> beginEndToRange (b,e) `isInsideR` rng_dp && t == MarkTime) (tagged^.tagList)
  let rng_time = beginEndToRange (b0,e0)
  z_tdp <- extractZipperByRange rng_time (tagged^.pennTree)
  z_tpp <- parent z_tdp
  guard (isChunkAs PP (current z_tpp))
  let (b_tpp,_e_tpp) = getRange (current z_tpp)
      rng_dp' = (b_dp,b_tpp-1)
      rng_head = let (b_h,e_h) = dp^.headX
                 in if e_h > b_tpp-1 then (b_h,b_tpp-1) else (b_h,e_h)
      dp' = dp & (headX .~ rng_head) . (maximalProjection .~ rng_dp')
  return (dp',[z_tpp])
