{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.),(^?),(%~),(&),to,_1,_2,_Nothing)
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
import           NLP.Type.PennTreebankII  (ChunkTag(..),POSTag(..),TernaryLogic(..)
                                          ,getRange,isNoun,posTag,tokenWord)
import           NLP.Type.TagPos          (TagPos(..))
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper,SplitType(..)
                                          ,Prep(..),PrepClass(..),DetP
                                          ,PP, AdjunctDP(..)
                                          ,TaggedLemma
                                          ,adjunct,headX,maximalProjection
                                          ,tokensByRange,mkOrdDP,mkPP,mkPPGerund
                                          ,mkSplittedDP,pennTree,tagList)
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs)
--
import Debug.Trace



mkPPFromZipper :: TaggedLemma t -> PrepClass -> Zipper t -> Maybe (PP t)
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



splitDP :: TaggedLemma t -> DetP t -> DetP t
splitDP tagged dp0 =
  let dp1 = fromMaybe dp0 $ do
              let rng0 = dp0^.maximalProjection
              z <- extractZipperByRange rng0 (tagged^.pennTree)
              z_pp <- childLast z
              guard (isChunkAs PP (current z_pp))
              pp <- mkPPFromZipper tagged PC_Other z_pp
              let (b_pp,_) = pp^.maximalProjection
              return (dp0 & (headX %~ (\(b,_) -> (b,b_pp-1)))
                          . (adjunct %~ (++ [AdjunctDP_PP pp])))
              
  in bareNounModifier tagged . fromMaybe dp1 $ do
       let rng1 = dp1^.maximalProjection
       z <- extractZipperByRange rng1 (tagged^.pennTree)
       guard (isChunkAs NP (current z))
       dp <- child1 z
       guard (isChunkAs NP (current dp))
       sbar <- next dp
       let rf = getRange . current
       ((guard (isChunkAs SBAR (current sbar)) >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
        (guard (isChunkAs VP (current sbar))   >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
        (splitParentheticalModifier tagged z))


splitParentheticalModifier :: TaggedLemma t -> Zipper t -> Maybe (DetP t)
splitParentheticalModifier tagged z = do
  guard (isChunkAs NP (current z))         -- dominating phrase must be NP
  dp1 <- child1 z
  guard (isChunkAs NP (current dp1))       -- first (head) phrase must be NP
  comma1 <- next dp1
  guard (isPOSAs M_COMMA (current comma1)) -- followed by comma
  z2 <- next comma1                        -- followed by a phrase
  -- followed by comma and end, or just end.
  ((do comma2 <- next z2
       guard (isPOSAs M_COMMA (current comma2))
       guard (is _Nothing (next comma2)))
   <|>
   (guard (is _Nothing (next z2))))

  let rf = getRange . current
  -- phrase inside parenthetical commas must be NP or clause
  ((guard (isChunkAs NP (current z2)) >> return (identApposHead tagged (rf dp1) (rf z2) z))
   <|>
   (guard (isChunkAs VP (current z2)) >> return (mkSplittedDP CLMod (rf dp1) (rf z2) z))
   <|>
   (guard (isChunkAs SBAR (current z2)) >> return (mkSplittedDP CLMod (rf dp1) (rf z2) z)))



identApposHead :: TaggedLemma t -> Range -> Range -> Zipper t -> DetP t
identApposHead tagged rng1 rng2 z = fromMaybe (mkSplittedDP APMod rng1 rng2 z) $
  ((do find (\(TagPos (b,e,t)) -> rng1 == beginEndToRange (b,e) && t == MarkEntity) (tagged^.tagList)
       return (mkSplittedDP APMod rng1 rng2 z))
   <|>
   (do find (\(TagPos (b,e,t)) -> rng2 == beginEndToRange (b,e) && t == MarkEntity) (tagged^.tagList)
       return (mkSplittedDP APMod rng2 rng1 z)))


-- | starting with capital letter
--
checkProperNoun :: TaggedLemma t -> Range -> Bool
checkProperNoun tagged (b,e) =
  let toks = tokensByRange tagged (b,e) -- (toList (current z))
  in (not.null) toks && isUpper (T.head (head toks))   -- unsafe!

-- | Identify bare noun subexpression inside noun phrase as modifier.
--   I did not implement the already-splitted case. We need multiple-adjunct
--   structure.
--
bareNounModifier :: TaggedLemma t --  (Lemma ': as)
                 -> DetP t -- (Lemma ': as)
                 -> DetP t -- (Lemma ': as)
bareNounModifier tagged x = fromMaybe x $ do
  let rng@(b0,_e0) = x^.maximalProjection
  z <- extractZipperByRange rng (tagged^.pennTree)
  guard (isChunkAs NP (current z))
  -- check entity for the last words
  let f (xb,xe) (yb,ye) = xe == ye && xb < yb && checkProperNoun tagged (yb,ye)
  TagPos (b1'',e1'',_t)
    <- find (\(TagPos (b1',e1',t)) -> f rng (beginEndToRange (b1',e1')) && t == MarkEntity) (tagged^.tagList)
  let (b1,e1) = beginEndToRange (b1'',e1'')
      idx_last_modifier_word = b1-1
  last_modifier_word <- find (\y -> y^._1 == idx_last_modifier_word) (toList (current z))
  guard (last_modifier_word^._2.to posTag.to isNoun == Yes)
  return (mkSplittedDP BNMod (b1,e1) (b0,idx_last_modifier_word) z)
