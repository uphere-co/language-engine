{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Preposition where

import           Control.Lens            ((^.),(^?),(.~),(&),_1,_2,_Right,to)
import           Control.Monad           (guard)
import           Data.List               (find)
import           Data.Maybe              (fromMaybe)
--
import           Data.Bitree             (_PL)
import           Data.BitreeZipper       (child1,current,next,parent)
import           Data.BitreeZipper.Util  (firstSiblingBy)
import           Data.Range              (isInsideR)
import           NLP.Type.PennTreebankII (ChunkTag(..),Lemma(..),POSTag(..)
                                         ,getRange,posTag,tokenWord)
import           NLP.Type.TagPos         (TagPos(..),TokIdx)
--
import           NLP.Syntax.Util         (beginEndToRange
                                         ,findZipperForRangeICP
                                         ,isChunkAs)
import           NLP.Syntax.Type         (MarkType(..))
import           NLP.Syntax.Type.XBar    (Zipper,DetP,CompVP(..),MaximalDP(..)
                                         ,Prep(..),PrepClass(..),PP,XP(..)
                                         ,complement,headX,maximalProjection
                                         ,original,mkOrdDP,mkPP
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





mkPPFromZipper :: PrepClass -> Zipper (Lemma ': as) -> Maybe (PP (Lemma ': as))
mkPPFromZipper pclass z = do
  guard (isChunkAs PP (current z))
  z_prep <- child1 z
  t <- z_prep ^? to current . _PL . _2 . to posTag
  guard (t == IN || t == TO)
  lma <- z_prep ^? to current . _PL . _2 . to tokenWord
  z_dp <- firstSiblingBy next (isChunkAs NP) z_prep
  return (mkPP (Prep_WORD lma,pclass) z (mkOrdDP z_dp))


identifyInternalTimePrep :: [TagPos TokIdx MarkType]
                         -> DetP t
                         -> (DetP t,[Zipper t])
identifyInternalTimePrep tagged dp = fromMaybe (dp,[]) $ do
  let z_dp = dp^.maximalProjection.original
      rng_dp@(b_dp,e_dp) = dp^.headX._1
  TagPos (b0,e0,_) <- find (\(TagPos (b,e,t)) -> beginEndToRange (b,e) `isInsideR` rng_dp && t == MarkTime) tagged
  let rng_time = beginEndToRange (b0,e0)
  z_tdp <- findZipperForRangeICP rng_time (current z_dp)
  z_tpp <- parent z_tdp
  guard (isChunkAs PP (current z_tpp))
  let rng_tpp@(b_tpp,e_tpp) = getRange (current z_tpp)
      rng_dp' = (b_dp,b_tpp-1)
      rng_head = let (b_h,e_h) = dp^.headX._2
                 in if e_h > b_tpp-1 then (b_h,b_tpp-1) else (b_h,e_h)
  let dp' = dp & (headX .~ (rng_dp',rng_head))
               . (maximalProjection .~ Seperated z_dp rng_dp')
      -- pp' = mkPP ( ,PC_Time)
  return (dp',[z_tpp])
