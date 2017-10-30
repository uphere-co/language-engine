{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Preposition where

import           Control.Applicative     ((<|>))
import           Control.Lens            ((^.),(^?),(.~),(&),_2,to)
import           Control.Monad           (guard)
import           Data.List               (find)
import           Data.Maybe              (fromMaybe)
--
import           Data.Bitree             (_PL)
import           Data.BitreeZipper       (child1,current,next,parent,toBitree,extractZipperByRange)
import           Data.BitreeZipper.Util  (firstSiblingBy)
import           Data.Range              (isInsideR,Range)
import           NLP.Type.PennTreebankII (ChunkTag(..),Lemma(..),POSTag(..)
                                         ,getRange,posTag,tokenWord)
import           NLP.Type.TagPos         (TagPos(..),TokIdx)
--
import           NLP.Syntax.Util         (beginEndToRange
                                         -- ,findZipperForRangeICP
                                         ,isChunkAs)
import           NLP.Syntax.Type         (MarkType(..))
import           NLP.Syntax.Type.XBar    (Zipper,DetP,CompVP(..)
                                         ,Prep(..),PrepClass(..),PP
                                         ,TaggedLemma(..),pennTree,tagList
                                         ,complement,headX,maximalProjection
                                         ,mkOrdDP,mkPP
                                         )


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
  let -- z = dp^.maximalProjection.original
      rng = dp^.maximalProjection -- getRange (current z)
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
      -- z = dp^.maximalProjection.original
      (prep,_pclass) = pp^.headX
      r = fromMaybe False $ do
            let rng = dp^.maximalProjection -- getRange (current z)
            find (isMatchedTime rng) (tagged^.tagList)
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
  return (mkPP (Prep_WORD lma,pclass) (getRange (current z)) (mkOrdDP z_dp))


identifyInternalTimePrep :: TaggedLemma t
                         -> DetP t
                         -> (DetP t,[Zipper t])
identifyInternalTimePrep tagged dp = fromMaybe (dp,[]) $ do
  let -- z_dp = dp^.maximalProjection.original
      rng_dp@(b_dp,_e_dp) = dp^.maximalProjection -- .maximal
  TagPos (b0,e0,_)
    <- find (\(TagPos (b,e,t)) -> beginEndToRange (b,e) `isInsideR` rng_dp && t == MarkTime) (tagged^.tagList)
  let rng_time = beginEndToRange (b0,e0)
  z_tdp <- extractZipperByRange rng_time (tagged^.pennTree) -- (current z_dp)
  z_tpp <- parent z_tdp
  guard (isChunkAs PP (current z_tpp))
  let (b_tpp,_e_tpp) = getRange (current z_tpp)
      rng_dp' = (b_dp,b_tpp-1)
      rng_head = let (b_h,e_h) = dp^.headX
                 in if e_h > b_tpp-1 then (b_h,b_tpp-1) else (b_h,e_h)
  dp' <- ((do z_dp' <- extractZipperByRange rng_dp' (tagged^.pennTree) -- (toBitree z_dp)
              return (dp & (headX .~ rng_head) . (maximalProjection .~ rng_dp'))
          )
          <|>
          (return (dp & (headX .~ rng_head) . (maximalProjection .~ rng_dp'))))

  return (dp',[z_tpp])
