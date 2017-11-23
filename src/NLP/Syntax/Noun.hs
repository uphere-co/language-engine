{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.),(^?),(.~),(%~),(&),to,_1,_2,_Just,_Nothing)
import           Control.Lens.Extras      (is)
import           Control.Monad            (guard)
import           Data.Char                (isUpper)
import           Data.Foldable            (foldrM,toList)
import           Data.List                (find,unfoldr)
import           Data.Maybe               (fromMaybe,listToMaybe)
import           Data.Monoid              (Last(..))
import qualified Data.Text           as T
--
import           Data.Attribute           (ahead)
import           Data.Bitree              (_PL)
import           Data.BitreeZipper        (child1,childLast,current,next,extractZipperByRange)
import           Data.BitreeZipper.Util   (firstSiblingBy)
import           Data.Range               (Range)
import           NLP.Type.PennTreebankII  (ChunkTag(..),POSTag(..),TernaryLogic(..),Lemma(..)
                                          ,getRange,isNoun,posTag,tokenWord,getAnnot)
import           NLP.Type.TagPos          (TagPos(..))
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper,SplitType(..)
                                          ,Prep(..),PrepClass(..),DetP
                                          ,PP, AdjunctDP(..), CompDP(..),HeadDP(..), SpecDP(..), HeadNP(..)
                                          ,DetClass(..),XP(..),TaggedLemma
                                          ,_MarkEntity,_NoDet,hn_range,hn_class
                                          ,adjunct,complement,headX,maximalProjection,specifier
                                          ,tokensByRange
                                          ,mkNP,mkOrdDP,mkSplittedDP,hd_range,hd_class,hn_range
                                          ,headRangeDP
                                          ,mkPP,mkPPGerund,hp_prep
                                          ,identifyArticle
                                          ,identifyPronounPerson
                                          ,pennTree,tagList)
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs,isLemmaAs,intLemma)
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
                                Prep_WORD "of" -> complement._Just.complement .~ (Just (CompDP_PP pp))
                                _              -> adjunct %~ (++ [AdjunctDP_PP pp])
              let (b_pp,_) = pp^.maximalProjection
              return (dp0 & (complement._Just.headX.hn_range %~ (\(b,_) -> (b,b_pp-1))) . ppreplace)

  in identifyDeterminer tagged . identifyNamedEntity tagged . bareNounModifier tagged . fromMaybe dp1 $ do
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
    z1 <- child1 z
    ((b1,e1),z1') <- rangeOfNPs z1
    comma1 <- next z1'
    guard (isPOSAs M_COMMA (current comma1)) -- followed by comma
    z2 <- next comma1                        -- followed by a phrase
    -- followed by comma and end, or just end.
    ((ba,ea),z_appos) <-
      ((do ((b2,e2),z2') <- (rangeOfNPs z2
                             <|> (guard (isChunkAs VP (current z2)) >> return (getRange (current z2), z2))
                             <|> (guard (isChunkAs SBAR (current z2)) >> return (getRange (current z2), z2))
                             <|> (guard (isChunkAs PP (current z2)) >> return (getRange (current z2), z2))
                            )
           comma2 <- next z2'
           guard (isPOSAs M_COMMA (current comma2))
           guard (is _Nothing (next comma2))
           return ((b2,e2),z2))
       <|>
       (do let showf = show . map (tokenWord.snd) . toList . current
           guard (isLemmaAs (Lemma "or") (current z2))
           z3 <- next z2
           ((b3,e3),z3') <- (rangeOfNPs z3
                             <|> (guard (isChunkAs VP (current z3)) >> return (getRange (current z3), z3))
                             <|> (guard (isChunkAs SBAR (current z3)) >> return (getRange (current z3), z3))
                             <|> (guard (isChunkAs PP (current z3)) >> return (getRange (current z3), z3))
                            )
           comma2 <- next z3'
           guard (isPOSAs M_COMMA (current comma2))
           guard (is _Nothing (next comma2))
           return ((b3,e3),z3))
       <|>
       (do ((b2,e2),z2') <- (rangeOfNPs z2
                             <|> (guard (isChunkAs VP (current z2)) >> return (getRange (current z2), z2))
                             <|> (guard (isChunkAs SBAR (current z2)) >> return (getRange (current z2), z2))
                             <|> (guard (isChunkAs PP (current z2)) >> return (getRange (current z2), z2))
                            )
           guard (is _Nothing (next z2'))
           return ((b2,e2),z2))
      )

    let rf = getRange . current
    -- phrase inside parenthetical commas must be NP or clause
    ((guard (isChunkAs NP (current z_appos))   >> return (identApposHead tagged (b1,e1) (ba,ea) z)) <|>
     (guard (isChunkAs VP (current z_appos))   >> return (mkSplittedDP CLMod    (b1,e1) (ba,ea) z)) <|>
     (guard (isChunkAs SBAR (current z_appos)) >> return (mkSplittedDP CLMod    (b1,e1) (ba,ea) z)) <|>
     (do guard (isChunkAs PP (current z_appos))
         pp <- mkPPFromZipper tagged PC_Other z_appos
         return (XP (HeadDP Nothing NoDet) (rf z) [] [AdjunctDP_PP pp] (Just (mkNP ((b1,e1),Nothing) Nothing)))))
  where
    step z = do
      guard (isChunkAs NP (current z))
      let (_,e) = getRange (current z)
      z' <- next z
      return ((e,z),z')

    rangeOfNPs z = do
      guard (isChunkAs NP (current z))
      let (b,e0) = getRange (current z)
          (e,z') = last ((e0,z):unfoldr step z)
      return ((b,e),z')


identApposHead :: TaggedLemma t -> Range -> Range -> Zipper t -> DetP t
identApposHead tagged rng1 rng2 z = fromMaybe (mkSplittedDP APMod rng1 rng2 z) $
  ((do find (\(TagPos (b,e,t)) -> rng1 == beginEndToRange (b,e) && is _MarkEntity t) (tagged^.tagList)
       return (mkSplittedDP APMod rng1 rng2 z))
   <|>
   (do find (\(TagPos (b,e,t)) -> rng2 == beginEndToRange (b,e) && is _MarkEntity t) (tagged^.tagList)
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
identifyDeterminer :: forall (t :: [*]) (as :: [*]) . (t ~ (Lemma ': as)) =>
                      TaggedLemma t -> DetP t -> DetP t
identifyDeterminer tagged dp = fromMaybe dp $ do
    rng <- dp^?complement._Just.headX.hn_range
    let zs = extractZipperByRange rng (tagged^.pennTree)
    (singleword zs <|> multiword zs)

  where
    singleword zs = do
      z <- find (isPOSAs PRP . current) zs
      (i,Lemma lma) <- intLemma z
      ptyp <- identifyPronounPerson lma
      (return . (headX.hd_range .~ Just (i,i)) . (headX.hd_class .~ Pronoun ptyp False) . (complement .~ Nothing)) dp
    --
    multiword zs = do
      z <- find (isChunkAs NP . current) zs
      (pronounGenOrArticle z <|>  cliticGen z)
    --
    pronounGenOrArticle z = do
      (i,att) <- listToMaybe (toList (current z))
      dtyp <- do let Lemma lma = ahead (getAnnot att)
                 ((do guard (posTag att == PRPDollar)
                      let Lemma lma = ahead (getAnnot att)
                      (\ptyp -> Pronoun ptyp True) <$> identifyPronounPerson lma)
                  <|>
                  (do guard (posTag att == DT)
                      identifyArticle lma))
      ( return
       .(headX.hd_range .~ Just (i,i))
       .(headX.hd_class .~ dtyp)
       .(complement._Just.headX.hn_range %~ (\(b,e) -> if b == i then (i+1,e) else (b,e)))
       .(complement._Just.maximalProjection %~ (\(b,e) -> if b == i then (i+1,e) else (b,e)))) dp
    --
    cliticGen z = do
      z1 <- child1 z
      guard (isChunkAs NP (current z1))
      z1last <- childLast z1
      guard (isPOSAs POS (current z1last))
      let (b,e) = getRange (current z1)
      ( return
       .(specifier %~ (SpDP_Gen (b,e-1) :) )
       .(headX.hd_range .~ Just (e,e))
       .(headX.hd_class .~ GenitiveClitic)
       .(complement._Just.headX.hn_range %~ (\(b',e') -> if b <= e then (e+1,e') else (b',e')))
       .(complement._Just.maximalProjection %~ (\(b',e') -> if b <= e then (e+1,e') else (b',e')))) dp


--
-- | Identify bare noun subexpression inside noun phrase as modifier.
--   I did not implement the already-splitted case. We need multiple-adjunct
--   structure.
--
bareNounModifier :: TaggedLemma t -> DetP t -> DetP t
bareNounModifier tagged dp = fromMaybe dp $ do
  rng@(b0,_e0) <- headRangeDP dp  -- dp^.maximalProjection
  z <- find (isChunkAs NP . current) (extractZipperByRange rng (tagged^.pennTree))
  -- check entity for the last words
  let f (xb,xe) (yb,ye) = xe == ye && xb < yb && checkProperNoun tagged (yb,ye)
  TagPos (b1'',e1'',_t)
    <- find (\(TagPos (b1',e1',t)) -> f rng (beginEndToRange (b1',e1')) && is _MarkEntity t) (tagged^.tagList)
  let (b1,e1) = beginEndToRange (b1'',e1'')
      idx_last_modifier_word = b1-1
  last_modifier_word <- find (\y -> y^._1 == idx_last_modifier_word) (toList (current z))
  guard (last_modifier_word^._2.to posTag.to isNoun == Yes)
  return (mkSplittedDP BNMod (b1,e1) (b0,idx_last_modifier_word) z)


--
-- | Set hn_class as identified
--
identifyNamedEntity :: TaggedLemma t -> DetP t -> DetP t
identifyNamedEntity tagged dp =
  fromMaybe dp $ do
    rng <- dp^?complement._Just.headX.hn_range
    TagPos (_,_,MarkEntity nec)
      <- find (\(TagPos (b,e,t)) -> rng == beginEndToRange (b,e) && is _MarkEntity t) (tagged^.tagList)
    (return . (complement._Just.headX.hn_class .~ (Just nec))) dp
