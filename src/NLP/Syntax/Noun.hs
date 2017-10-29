{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.),(^?),to,_1,_2)
import           Control.Monad            (guard)
import           Data.Char                (isUpper)
import           Data.Foldable            (toList)
import           Data.List                (find)
import           Data.Maybe               (fromMaybe,isNothing)
import qualified Data.Text           as T
--
import           Data.BitreeZipper        (child1,current,next)
import           Data.Range               (Range)
import           NLP.Type.PennTreebankII  (ChunkTag(..),Lemma,POSTag(..),TernaryLogic(..)
                                          ,getRange,isNoun,posTag)
import           NLP.Type.TagPos          (TagPos(..),TokIdx)
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper,SplitType(..),DetP,maximalProjection
                                          ,tokensByRange,mkSplittedDP,original,_Intact
                                          )
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs)


splitDP :: [TagPos TokIdx MarkType]
        -> DetP (Lemma ': as)
        -> DetP (Lemma ': as)
splitDP tagged dp0 =
  bareNounModifier tagged . fromMaybe dp0 $ do
    (z,_) <- dp0^?maximalProjection._Intact
    guard (isChunkAs NP (current z))
    dp <- child1 z
    guard (isChunkAs NP (current dp))
    sbar <- next dp
    let rf = getRange . current
    ((guard (isChunkAs SBAR (current sbar)) >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
     (guard (isChunkAs VP (current sbar))   >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
     (splitParentheticalModifier tagged z))


splitParentheticalModifier :: [TagPos TokIdx MarkType] -> Zipper (Lemma ': as) -> Maybe (DetP (Lemma ': as))
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
       guard (isNothing (next comma2)))
   <|>
   (guard (isNothing (next z2))))

  let rf = getRange . current
  -- phrase inside parenthetical commas must be NP or clause
  ((guard (isChunkAs NP (current z2)) >> return (identApposHead tagged (rf dp1) (rf z2) z))
   <|>
   (guard (isChunkAs VP (current z2)) >> return (mkSplittedDP CLMod (rf dp1) (rf z2) z))
   <|>
   (guard (isChunkAs SBAR (current z2)) >> return (mkSplittedDP CLMod (rf dp1) (rf z2) z)))



identApposHead :: [TagPos TokIdx MarkType] -> Range -> Range -> Zipper t -> DetP t
identApposHead tagged rng1 rng2 z = fromMaybe (mkSplittedDP APMod rng1 rng2 z) $
  ((do find (\(TagPos (b,e,t)) -> rng1 == beginEndToRange (b,e) && t == MarkEntity) tagged
       return (mkSplittedDP APMod rng1 rng2 z))
   <|>
   (do find (\(TagPos (b,e,t)) -> rng2 == beginEndToRange (b,e) && t == MarkEntity) tagged
       return (mkSplittedDP APMod rng2 rng1 z)))


-- | starting with capital letter
--
checkProperNoun :: Zipper t -> Range -> Bool
checkProperNoun z (b,e) =
  let toks = tokensByRange (b,e) (toList (current z))
  in (not.null) toks && isUpper (T.head (head toks))

-- | Identify bare noun subexpression inside noun phrase as modifier.
--   I did not implement the already-splitted case. We need multiple-adjunct
--   structure.
--
bareNounModifier :: [TagPos TokIdx MarkType]
                 -> DetP (Lemma ': as)
                 -> DetP (Lemma ': as)
bareNounModifier tagged x = fromMaybe x $ do
  let z = x^.maximalProjection.original
  guard (isChunkAs NP (current z))
  let rng@(b0,_e0) = getRange (current z)
  -- check entity for the last words
  let f z' (xb,xe) (yb,ye) = xe == ye && xb < yb && checkProperNoun z' (yb,ye)
  TagPos (b1'',e1'',_t) <- find (\(TagPos (b1',e1',t)) -> f z rng (beginEndToRange (b1',e1')) && t == MarkEntity) tagged
  let (b1,e1) = beginEndToRange (b1'',e1'')
      idx_last_modifier_word = b1-1
  last_modifier_word <- find (\y -> y^._1 == idx_last_modifier_word) (toList (current z))
  guard (last_modifier_word^._2.to posTag.to isNoun == Yes)
  return (mkSplittedDP BNMod (b1,e1) (b0,idx_last_modifier_word) z)
