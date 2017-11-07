{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.),(^?),(.~),(%~),(&),to,_1,_2,_Nothing)
import           Control.Lens.Extras      (is)
import           Control.Monad            (guard)
import           Data.Char                (isUpper)
import           Data.Foldable            (toList)
import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text           as T
--
import           Data.Bitree              (_PL)
import           Data.BitreeZipper        (child1,childLast,current,next,extractZipperByRange)
import           Data.BitreeZipper.Util   (firstSiblingBy)
import           Data.Range               (Range)
import           NLP.Type.PennTreebankII  (ChunkTag(..),POSTag(..),TernaryLogic(..),Lemma(..)
                                          ,getRange,isNoun,posTag,tokenWord)
import           NLP.Type.TagPos          (TagPos(..))
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper,SplitType(..)
                                          ,Prep(..),PrepClass(..),DetP
                                          ,PP, AdjunctDP(..), CompDP(..),HeadDP(..), NomClass(..)
                                          ,XP(..)
                                          ,TaggedLemma
                                          ,adjunct,complement,headX,maximalProjection
                                          ,tokensByRange
                                          ,mkOrdDP,mkSplittedDP,hd_range,hd_class
                                          ,mkPP,mkPPGerund,hp_prep
                                          ,pennTree,tagList)
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs,isLemmaAs)
--

import Debug.Trace

mkPPFromZipper :: TaggedLemma (Lemma ': as) -> PrepClass -> Zipper (Lemma ': as) -> Maybe (PP (Lemma ': as))
mkPPFromZipper tagged pclass z = do
  guard (isChunkAs PP (current z))
  z_prep <- child1 z
  t <- z_prep ^? to current . _PL . _2 . to posTag
  guard (t == IN || t == TO)
  lma <- z_prep ^? to current . _PL . _2 . to tokenWord
  ((do z_dp <- firstSiblingBy next (isChunkAs NP) z_prep
       return (mkPP (Prep_WORD lma,pclass) (getRange (current z)) (splitDP tagged (mkOrdDP z_dp))))
   <|>
   (do z_s <- firstSiblingBy next (isChunkAs S) z_prep
       z_vp <- child1 z_s
       guard (isChunkAs VP (current z_vp))
       z_v <- child1 z_vp
       guard (isPOSAs VBG (current z_v))
       return (mkPPGerund (Prep_WORD lma,pclass) (getRange (current z)) z_s)))



splitDP :: TaggedLemma (Lemma ': as) -> DetP (Lemma ': as) -> DetP (Lemma ': as)
splitDP tagged dp0 =
  let dp1 = fromMaybe dp0 $ do
              let rng0 = dp0^.maximalProjection

              z <- find (isChunkAs NP . current) (extractZipperByRange rng0 (tagged^.pennTree))
              z_pp <- childLast z
              guard (isChunkAs PP (current z_pp))
              pp <- mkPPFromZipper tagged PC_Other z_pp
              let ppreplace = case pp^.headX.hp_prep of
                                Prep_WORD "of" -> complement .~ (Just (CompDP_PP pp))
                                _              -> adjunct %~ (++ [AdjunctDP_PP pp])
              let (b_pp,_) = pp^.maximalProjection
              return (dp0 & (headX.hd_range %~ (\(b,_) -> (b,b_pp-1))) . ppreplace)

  in identifyPronoun tagged . bareNounModifier tagged . fromMaybe dp1 $ do
       let rng1 = dp1^.maximalProjection
       z <- find (isChunkAs NP . current) (extractZipperByRange rng1 (tagged^.pennTree))
       dp <- child1 z
       guard (isChunkAs NP (current dp))
       sbar <- next dp
       let rf = getRange . current
       ((guard (isChunkAs SBAR (current sbar)) >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
        (guard (isChunkAs VP (current sbar))   >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
        (splitParentheticalModifier tagged z))


splitParentheticalModifier :: TaggedLemma (Lemma ': as) -> Zipper (Lemma ': as) -> Maybe (DetP (Lemma ': as))
splitParentheticalModifier tagged z = do
  guard (isChunkAs NP (current z))         -- dominating phrase must be NP
  dp1 <- child1 z
  guard (isChunkAs NP (current dp1))       -- first (head) phrase must be NP
  comma1 <- next dp1
  guard (isPOSAs M_COMMA (current comma1)) -- followed by comma
  z2 <- next comma1                        -- followed by a phrase
  -- followed by comma and end, or just end.
  z_appos <-
    ((do comma2 <- next z2
         guard (isPOSAs M_COMMA (current comma2))
         guard (is _Nothing (next comma2))
         return z2)
     <|>
     (do let showf = show . map (tokenWord.snd) . toList . current
         guard (isLemmaAs (Lemma "or") (current z2))
         z3 <- next z2
         comma2 <- next z3
         guard (isPOSAs M_COMMA (current comma2))
         guard (is _Nothing (next comma2))
         return z3)
     <|> 
     (do guard (is _Nothing (next z2))
         return z2)
    )

  let rf = getRange . current
  -- phrase inside parenthetical commas must be NP or clause
  ((guard (isChunkAs NP (current z_appos)) >> return (identApposHead tagged (rf dp1) (rf z_appos) z)) <|>
   (guard (isChunkAs VP (current z_appos)) >> return (mkSplittedDP CLMod (rf dp1) (rf z_appos) z))    <|>
   (guard (isChunkAs SBAR (current z_appos)) >> return (mkSplittedDP CLMod (rf dp1) (rf z_appos) z))  <|>
   (do guard (isChunkAs PP (current z_appos))
       pp <- mkPPFromZipper tagged PC_Other z_appos
       return (XP (HeadDP (rf dp1) RExp) (rf z) Nothing [AdjunctDP_PP pp] Nothing)))



identApposHead :: TaggedLemma t -> Range -> Range -> Zipper t -> DetP t
identApposHead tagged rng1 rng2 z = fromMaybe (mkSplittedDP APMod rng1 rng2 z) $
  ((do find (\(TagPos (b,e,t)) -> rng1 == beginEndToRange (b,e) && t == MarkEntity) (tagged^.tagList)
       return (mkSplittedDP APMod rng1 rng2 z))
   <|>
   (do find (\(TagPos (b,e,t)) -> rng2 == beginEndToRange (b,e) && t == MarkEntity) (tagged^.tagList)
       return (mkSplittedDP APMod rng2 rng1 z)))

--
-- | starting with capital letter
--
checkProperNoun :: TaggedLemma t -> Range -> Bool
checkProperNoun tagged (b,e) =
  let toks = tokensByRange tagged (b,e)
  in (not.null) toks && isUpper (T.head (head toks))   -- unsafe!


--
-- | check whether DP is pronoun and change NomClass accordingly
-- 
identifyPronoun :: TaggedLemma t -> DetP t -> DetP t
identifyPronoun tagged dp = fromMaybe dp $ do
  let rng = dp^.headX.hd_range
  find (isPOSAs PRP . current) (extractZipperByRange rng (tagged^.pennTree))
  (return . (headX.hd_class .~ Pronoun)) dp




-- | Identify bare noun subexpression inside noun phrase as modifier.
--   I did not implement the already-splitted case. We need multiple-adjunct
--   structure.
--
bareNounModifier :: TaggedLemma t -> DetP t -> DetP t
bareNounModifier tagged x = fromMaybe x $ do
  let rng@(b0,_e0) = x^.maximalProjection
  z <- find (isChunkAs NP . current) (extractZipperByRange rng (tagged^.pennTree))
  -- check entity for the last words
  let f (xb,xe) (yb,ye) = xe == ye && xb < yb && checkProperNoun tagged (yb,ye)
  TagPos (b1'',e1'',_t)
    <- find (\(TagPos (b1',e1',t)) -> f rng (beginEndToRange (b1',e1')) && t == MarkEntity) (tagged^.tagList)
  let (b1,e1) = beginEndToRange (b1'',e1'')
      idx_last_modifier_word = b1-1
  last_modifier_word <- find (\y -> y^._1 == idx_last_modifier_word) (toList (current z))
  guard (last_modifier_word^._2.to posTag.to isNoun == Yes)
  return (mkSplittedDP BNMod (b1,e1) (b0,idx_last_modifier_word) z)
