{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module NLP.Syntax.Noun where

import           Control.Applicative      ((<|>))
import           Control.Lens             ((^.),(^..),(^?),(.~),(%~),(&),to,_1,_2
                                          ,_Left,_Just,_Nothing,folded,filtered)
import           Control.Lens.Extras      (is)
import           Control.Monad            (guard)
import           Data.Char                (isUpper)
import           Data.Foldable            (toList)
import qualified Data.HashSet        as HS
import           Data.List                (find,insert,sort,unfoldr)
import           Data.Maybe               (fromMaybe,listToMaybe)
import qualified Data.Text           as T
--
import           Data.Attribute           (ahead)
import           Data.Bitree              (_PL)
import           Data.BitreeZipper        (child1,childLast,current,next,parent
                                          ,extractZipperByRange)
import           Data.BitreeZipper.Util   (firstSiblingBy)
import           Data.Range               (isInside,isInsideR,Range)
import           NLP.Type.PennTreebankII  (ChunkTag(..),POSTag(..),TernaryLogic(..),Lemma(..)
                                          ,getRange,isNoun,posTag,tokenWord,getAnnot)
import           NLP.Type.TagPos          (TagPos(..))
--
import           NLP.Syntax.Type          (MarkType(..))
import           NLP.Syntax.Type.XBar     (Zipper,SplitType(..)
                                          ,Prep(..),PrepClass(..),DetP
                                          ,PP, AdjunctDP(..), CompDP(..),HeadDP(..),SpecDP(..)
                                          ,DetClass(..),XP(..),TaggedLemma, AdjunctVP(..)
                                          ,PrepClass(..), CPDPPP(..)
                                          ,PPTree(..), DPTree(..), _PPTree, _DPTree
                                          ,_MarkEntity,_AdjunctDP_PP
                                          ,hn_range,hn_class
                                          ,adjunct,complement,headX,maximalProjection,specifier
                                          ,tokensByRange
                                          ,mkNP,mkOrdDP,mkSplittedDP,hd_range,hd_class,hn_range
                                          ,headRangeDP
                                          ,mkPP,mkPPGerund,hp_prep
                                          ,identifyArticle
                                          ,identifyPronounPerson
                                          ,pennTree,tagList)
import           NLP.Syntax.Util          (beginEndToRange,isChunkAs,isPOSAs,isLemmaAs,intLemma
                                          ,rootTag)
--
import Debug.Trace



mkPPFromZipper :: TaggedLemma (Lemma ': as) -> PrepClass -> Zipper (Lemma ': as) -> Maybe PPTree
mkPPFromZipper tagged pclass z = do
  guard (isChunkAs PP (current z))
  z_prep <- child1 z
  t <- z_prep ^? to current . _PL . _2 . to posTag
  guard (t == IN || t == TO)
  lma <- z_prep ^? to current . _PL . _2 . to tokenWord
  ((do z_dp <- firstSiblingBy next (isChunkAs NP) z_prep
       let dptree = splitDP tagged (DPTree (mkOrdDP z_dp) [])
           pp = mkPP (Prep_WORD lma,pclass) (getRange (current z)) (dptree^._DPTree._1)
       return (PPTree pp (Just dptree)))
   <|>
   (do z_s <- firstSiblingBy next (isChunkAs S) z_prep
       z_vp <- child1 z_s
       guard (isChunkAs VP (current z_vp))
       z_v <- child1 z_vp
       guard (isPOSAs VBG (current z_v))
       let pp = mkPPGerund (Prep_WORD lma,pclass) (getRange (current z)) z_s
       return (PPTree pp Nothing)))





splitDP :: TaggedLemma (Lemma ': as) -> DPTree -> DPTree
splitDP tagged (DPTree dp0 lst0) =
  let dptr1 =
        fromMaybe (DPTree dp0 lst0) $ do
              let rng0 = dp0^.maximalProjection

              z <- find (isChunkAs NP . current) (extractZipperByRange rng0 (tagged^.pennTree))
              -- adjunct only one.. but this must be recursive.
              z_last <- childLast z
              tag <- (rootTag (current z_last))^?_Left
              case tag of
                PP -> do pptr@(PPTree pp _) <- mkPPFromZipper tagged PC_Other z_last   -- for the time being
                         let rng_pp = pp^.maximalProjection
                             ppreplace = case pp^.headX.hp_prep of
                                           Prep_WORD "of" -> complement._Just.complement .~ (Just (CompDP_PP rng_pp))
                                           _              -> adjunct %~ (addPP rng_pp) -- . removePP (pp^.maximalProjection)
                             (b_pp,_) = pp^.maximalProjection
                             dp = dp0 & (complement._Just.headX.hn_range %~ (\(b,_) -> (b,b_pp-1))) . ppreplace
                         return (DPTree dp [pptr])
                ADJP -> let (b_ap,e_ap) = getRange (current z_last)
                            apreplace = adjunct %~ (++ [AdjunctDP_AP (b_ap,e_ap)])
                            dp = dp0 & (complement._Just.headX.hn_range %~ (\(b,_) -> (b,b_ap-1))) . apreplace
                        in return (DPTree dp [])
                _ -> Nothing
      dptr2 = identifyInternalTimePrep tagged dptr1
  in dptr2 & (_DPTree._1) %~

                 (identifyDeterminer tagged
                 .identifyNamedEntity tagged
                 .bareNounModifier tagged
                 .(\dp1 -> fromMaybe dp1 (identifyClausalModifier tagged dp1)))



identifyClausalModifier :: TaggedLemma (Lemma ': as) -> DetP -> Maybe DetP
identifyClausalModifier tagged dp0 = do
  let (b0,e0) = dp0^.maximalProjection
      e0' = (\case [] -> e0; (e:_) -> e-1) (sort (dp0^..adjunct.traverse._AdjunctDP_PP._1))
      rng0 = (b0,e0')
      rf = getRange . current

  z <- find (isChunkAs NP . current) (extractZipperByRange rng0 (tagged^.pennTree))
  ((do dp <- child1 z
       guard (isChunkAs NP (current dp))
       sbar <- next dp
       ((guard (isChunkAs SBAR (current sbar)) >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
        (guard (isChunkAs S (current sbar)) >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
        (guard (isChunkAs VP (current sbar))   >> return (mkSplittedDP CLMod (rf dp) (rf sbar) z)) <|>
        (splitParentheticalModifier tagged z)))
   <|>
   (do sbar <- childLast z
       guard (isChunkAs S (current sbar))
       let (b,_e) = rng0
           (b1,e1) = rf sbar
       return (mkSplittedDP CLMod (b,b1-1) (b1,e1) z)))


splitParentheticalModifier :: TaggedLemma (Lemma ': as) -> Zipper (Lemma ': as) -> Maybe DetP
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
                             <|> (guard (isChunkAs S (current z2)) >> return (getRange (current z2), z2))
                             <|> (guard (isChunkAs SBAR (current z2)) >> return (getRange (current z2), z2))
                             <|> (guard (isChunkAs PP (current z2)) >> return (getRange (current z2), z2))
                            )
           comma2 <- next z2'
           guard (isPOSAs M_COMMA (current comma2))
           guard (is _Nothing (next comma2))
           return ((b2,e2),z2))
       <|>
       (do guard (isLemmaAs (Lemma "or") (current z2))
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
         PPTree pp _ <- mkPPFromZipper tagged PC_Other z_appos -- for the time being
         let rng_pp = pp^.maximalProjection
         return (XP (HeadDP Nothing NoDet) (rf z) [] [AdjunctDP_PP rng_pp] (Just (mkNP ((b1,e1),Nothing) Nothing)))))


rangeOfNPs :: Zipper t -> Maybe (Range,Zipper t)
rangeOfNPs z0 = do
    guard (isChunkAs NP (current z0))
    let (b,e0) = getRange (current z0)
        (e,z') = last ((e0,z0):unfoldr step z0)
    return ((b,e),z')
  where
    step z = do
      guard (isChunkAs NP (current z))
      let (_,e) = getRange (current z)
      z' <- next z
      return ((e,z),z')



identApposHead :: TaggedLemma t -> Range -> Range -> Zipper t -> DetP
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
identifyDeterminer :: TaggedLemma (Lemma ': as) -> DetP -> DetP
identifyDeterminer tagged dp = fromMaybe dp $ do
    let rng = dp^.maximalProjection
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
                      let Lemma lma' = ahead (getAnnot att)
                      (\ptyp -> Pronoun ptyp True) <$> identifyPronounPerson lma')
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
      rng_np <- dp^?complement._Just.headX.hn_range
      let ps = (filter (\x -> (x^._1) `isInside` rng_np) . toList . current) z
      (i,_) <- find (\(_,y) -> posTag y == POS) ps
      ( return
       .(specifier %~ (SpDP_Gen (rng_np^._1,i-1) :) )
       .(headX.hd_range .~ Just (i,i))
       .(headX.hd_class .~ GenitiveClitic)
       .(complement._Just.headX.hn_range %~ (\(_b',e') -> (i+1,e')))
       .(complement._Just.maximalProjection %~ (\(_b',e') -> (i+1,e')))) dp


--
-- | Identify bare noun subexpression inside noun phrase as modifier.
--   I did not implement the already-splitted case. We need multiple-adjunct
--   structure.
--
bareNounModifier :: TaggedLemma t -> DetP -> DetP
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
identifyNamedEntity :: TaggedLemma t -> DetP -> DetP
identifyNamedEntity tagged dp =
  fromMaybe dp $ do
    rng <- dp^?complement._Just.headX.hn_range
    TagPos (_,_,MarkEntity nec)
      <- find (\(TagPos (b,e,t)) -> rng == beginEndToRange (b,e) && is _MarkEntity t) (tagged^.tagList)
    (return . (complement._Just.headX.hn_class .~ (Just nec))) dp


--
-- |
--
identifyInternalTimePrep :: TaggedLemma (Lemma ': as)
                         -> DPTree -- DetP
                         -> DPTree -- (DetP,[AdjunctVP])
identifyInternalTimePrep tagged (DPTree dp lst) = fromMaybe (DPTree dp lst) $ do
  let rng_dp@(b_dp,_e_dp) = dp^.maximalProjection
  TagPos (b0,e0,_)
    <- find (\(TagPos (b,e,t)) -> beginEndToRange (b,e) `isInsideR` rng_dp && t == MarkTime) (tagged^.tagList)
  let rng_time = beginEndToRange (b0,e0)
  z_tdp <- find (isChunkAs NP . current) (extractZipperByRange rng_time (tagged^.pennTree))
  z_tpp <- parent z_tdp
  let rng_tpp = getRange (current z_tpp)
  guard (rng_tpp `isInsideR` rng_dp)
  guard (isChunkAs PP (current z_tpp))

  let (b_tpp,_e_tpp) = getRange (current z_tpp)
      rng_dp' = (b_dp,b_tpp-1)
  (b_h,e_h) <- dp^?complement._Just.headX.hn_range
  let rng_head = if e_h > b_tpp-1 then (b_h,b_tpp-1) else (b_h,e_h)
  pptree@(PPTree tpp _) <- mkPPFromZipper tagged PC_Time z_tpp  -- for the time being
  let dp' = dp & -- (maximalProjection .~ rng_dp')
                  (complement._Just.headX.hn_range .~ rng_head)

               -- . (complement._Just.maximalProjection .~ rng_dp')
                  . (adjunct %~ addPP (tpp^.maximalProjection))

  return (DPTree dp' (addPPTree pptree lst))  --  ++ pptree tpp  [AdjunctVP_PP tpp])



removePP :: Range -> [AdjunctDP] -> [AdjunctDP]
removePP rng xs = xs^..folded.filtered (\x->x^?_AdjunctDP_PP /= Just rng)



addPP :: Range -> [AdjunctDP] -> [AdjunctDP]
addPP rng xs = (sort . HS.toList . HS.insert (AdjunctDP_PP rng) . HS.fromList) xs


addPPTree :: PPTree -> [PPTree] -> [PPTree]
addPPTree pptree@(PPTree pp _) pplst =
  let rng = pp^.maximalProjection
      (xs,ys) = break (\pptree' -> rng == pptree'^._PPTree._1.maximalProjection) pplst
      ys' = filter (\pptree' -> rng /= pptree'^._PPTree._1.maximalProjection) ys
  in xs ++ (pptree : ys')
--   insert (AdjunctDP_PP rng) xs
