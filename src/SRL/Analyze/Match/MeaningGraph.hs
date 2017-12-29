{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze.Match.MeaningGraph where

import           Control.Applicative          ((<|>))
import           Control.Lens
import           Control.Lens.Extras          (is)
import           Control.Monad                (guard)
import           Data.Bifoldable              (biList)
import           Data.Function                (on)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (find,groupBy,sortBy,intercalate)
import           Data.Maybe                   (fromMaybe,mapMaybe,maybeToList)
import qualified Data.Text              as T
import           Data.Text                    (Text)
--
import           Data.Bitree                  (getRoot1)
import           Data.BitreeZipper            (current)
import           Data.Range                   (Range,elemRevIsInsideR,isInsideR)
import           Lexicon.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util              (GetIntLemma(..),intLemma0)
import           NLP.Type.PennTreebankII
--
import           SRL.Analyze.Match.Entity
import           SRL.Analyze.Match.Frame
import           SRL.Analyze.Match.Preposition (ppRelFrame)
import           SRL.Analyze.Type             (MGVertex(..),MGEdge(..),MeaningGraph(..)
                                              ,SentStructure,AnalyzePredata
                                              ,PredicateInfo(..)
                                              ,VertexMap(..)
                                              ,vm_rangeToIndex
                                              -- ,vm_hnrangeToIndex
                                              ,vm_rangeDependency
                                              ,vm_headRangeToFullRange
                                              ,analyze_framedb
                                              ,_PredAppos,_MGEntity,_MGPredicate
                                              ,ss_tagged,ss_x'trs
                                              ,me_relation,mv_range,mv_id,mg_vertices,mg_edges)

import           SRL.Analyze.Type.Match       (DPInfo(..), EntityInfo(..),FrameMatchResult(..)
                                              ,adi_appos,adi_compof,adi_coref,adi_poss,adi_adjs
                                              ,ei_fullRange,ei_headRange,ei_prep,ei_isClause,ei_isTime
                                              )


import Debug.Trace

import NLP.Syntax.Format.Internal



dependencyOfX'Tree :: X'Tree 'PH1 -> [(Range,Range)]
dependencyOfX'Tree (PN (rng0,_) xs) = map ((rng0,) . fst . getRoot1) xs ++ concatMap dependencyOfX'Tree xs
dependencyOfX'Tree (PL _)           = []



mkEntityFun :: (EntityInfo,DPInfo) -> [(Int -> MGVertex)]
mkEntityFun (EI rng rnghead _mprep txt _ _,di) =
  let mkRel frm (EI rng' rng'' _ txt' False _) = [ \i'  -> MGEntity i' (Just rng') (Just rng'') txt' []
                                                 , \i'' -> MGPredicate i'' (Just rng') frm PredAppos ]
      mkRel frm (EI rng' rng'' _ txt' True _)  = [ \i'' -> MGPredicate i'' (Just rng') frm PredAppos ]

      appos = maybe [] (mkRel "Instance") (di^.adi_appos)
      comp = do c <- maybeToList (di^.adi_compof)
                if (c^.ei_isClause)
                  then mkRel "Purpose" c
                  else mkRel "Partitive" c
      poss = concatMap (mkRel "Possession") (di^.adi_poss)
      adjs = do a <- di^.adi_adjs
                p <- maybeToList (a^.ei_prep)
                f <- if (a^.ei_isTime)
                       then [("Time_vector","Event","Landmark_event")]
                       else maybeToList (ppRelFrame p)
                mkRel (f^._1) a
  in (\i -> MGEntity i (Just rng) (Just rnghead) txt []) : (appos ++ comp ++ poss ++ adjs)


mkMGVertices :: (PreAnalysis '[Lemma],[(Range,Range)])
             -> ([(Range,VerbProperty (Zipper '[Lemma]),X'Tree 'PH1,FrameMatchResult,Maybe (SenseID,Bool))]
                ,[(Lemma,Lemma,X'Tree 'PH1,(FNFrame,Range),(FNFrameElement,Maybe EntityInfo),(FNFrameElement,EntityInfo))]
                )

             -> ([MGVertex]
                ,[(EntityInfo,DPInfo)]
                ,[((Int,FNFrame,Text,[(FNFrameElement,(Bool,Range))]),MGVertex)]
                ,[(Range,Range)]
                )
mkMGVertices (tagged,depmap) (matched,nmatched) =
  let preds = flip map matched $ \(rng,vprop,x'tr,FMR idiom frm _ _,sense) i
                                   -> MGPredicate i (Just rng) frm (PredVerb idiom sense (simplifyVProp vprop))
      npreds = flip map nmatched $ \(lma,verb,x'tr,(frm,rng_dp),_,_) ->
                                  \i -> MGPredicate i (Just rng_dp) frm (PredNominalized lma verb)
      ipreds = zipWith ($) (preds ++ npreds) [1..]

      ett_verb :: [(EntityInfo,DPInfo)]
      ett_verb  = do (rng,_,x'tr,FMR _ _ mselected _,_) <- matched
                     (_,felst) <- maybeToList mselected
                     (_fe,x) <- felst
                     case x of
                       CompVP_CP _cp -> [] -- CP is not an entity.
                       CompVP_AP rng_ap -> do
                         ap <- maybeToList (cpdpppFromX'Tree x'tr rng_ap _APCase)
                         return (entityFromAP tagged ap)
                       CompVP_DP rng_dp -> do
                         dp <- maybeToList (cpdpppFromX'Tree x'tr rng_dp _DPCase)
                         let y@(ei,_) = entityFromDP x'tr tagged dp
                             rng' = ei^.ei_fullRange
                         if is _Just (find (== (rng',rng)) depmap)
                           then []
                           else return y
                       CompVP_PP rng_pp -> maybeToList $ do
                         pp <- cpdpppFromX'Tree x'tr rng_pp _PPCase
                         rng_dp <- pp^?complement._CompPP_DP
                         dp <- cpdpppFromX'Tree x'tr rng_dp _DPCase
                         return (entityFromDP x'tr tagged dp)

      filterFrame = filter (\(ei,_) -> not (any (\p -> p^.mv_range == Just (ei^.ei_fullRange)) ipreds))
      --

      ett_nominal :: [(EntityInfo,DPInfo)]
      ett_nominal = do (_lma,_verb,x'tr,(_frm,_rng_dp),(_subj,mei_subj),(_obj,ei_obj)) <- nmatched
                       let lstsubj = case mei_subj of
                                       Just ei_subj -> [(ei_subj,DI Nothing Nothing Nothing [] [])]
                                       _ -> []
                       (ei_obj,DI Nothing Nothing Nothing [] []) : lstsubj


      ett_verbnom = filterFrame
                  . map head
                  . groupBy ((==) `on` (^._1.ei_fullRange))
                  . sortBy (compare `on` (^._1.ei_fullRange))
                  $ (ett_verb ++ ett_nominal)



      ettfunc_verbnom = concatMap mkEntityFun ett_verbnom

      ettfunc_prep = do (_,_,x'tr,FMR _ _ _ lst,_) <- matched
                        (frm,prep,felst) <- lst
                        return (\i -> ((i,frm,prep,felst),MGPredicate i Nothing frm (PredPrep prep)))

      n_ipreds = length ipreds
      n_ettverbnom = length ettfunc_verbnom
      -- n_ettprep = length ettfunc_prep
      iett_verbnom = zipWith ($) ettfunc_verbnom (enumFrom (n_ipreds+1))
      iett_prep = zipWith ($) ettfunc_prep (enumFrom (n_ipreds+n_ettverbnom+1))
      vertices = ipreds ++ iett_verbnom ++ (map snd iett_prep)
      headfull = flip concatMap ett_verbnom $ \(ei,di) ->
                   let appos = di^..adi_appos._Just
                       compof = di^..adi_compof._Just
                       poss = di^.adi_poss
                       adjs = di^.adi_adjs
                       eis = ei : (appos ++ compof ++ poss ++ adjs)
                   in map (\e -> (e^.ei_headRange,e^.ei_fullRange)) eis
                    {- <- ett_verbnom
                    <-
                    return (ei^.ei_headRange,ei^.ei_fullRange) -}
  in -- trace (intercalate "\n" (map show ett_verb) ++ "\n" ++ intercalate "\n" (map show vertices)) $
       (vertices,ett_verbnom,iett_prep,headfull)


mkRoleEdges :: VertexMap
            -> [(Range,VerbProperty (Zipper '[Lemma]),X'Tree 'PH1,FrameMatchResult,Maybe (SenseID,Bool))]
            -> [MGEdge]
mkRoleEdges vmap matched = do
  let rngidxmap = vmap^.vm_rangeToIndex
      depmap = vmap^.vm_rangeDependency
      headfull = vmap^.vm_headRangeToFullRange
  (rng,_,x'tr,FMR _ _ mselected _,_) <- matched
  i <- maybeToList (HM.lookup (0,Just rng) rngidxmap)   -- frame
  (_,felst) <- maybeToList mselected
  (fe,x) <- felst
  (rng',mprep) <- case x of
                    CompVP_CP rng_cp -> do
                      cp <- maybeToList (cpdpppFromX'Tree x'tr rng_cp _CPCase)
                      let mprep = case cp^.headX of
                                    C_PHI -> Nothing
                                    C_WORD prep -> if prep == Lemma "that" then Nothing else return (unLemma prep)
                      return (rng_cp,mprep)
                    CompVP_DP rng_dp -> return (rng_dp,Nothing)
                    CompVP_AP rng_ap -> return (rng_ap,Nothing)                    
                    CompVP_PP rng_pp -> do
                      pp <- maybeToList (cpdpppFromX'Tree x'tr rng_pp _PPCase)
                      return (pp^.complement.to (compPPToRange SPH1),pp^?headX.hp_prep._Prep_WORD)
  let rng'full = fromMaybe rng' (lookup rng' headfull)
  i' <- maybeToList (HM.lookup (0,Just rng'full) rngidxmap)  -- frame element
  let b = is _Just (find (== (rng',rng)) depmap)
  return (MGEdge fe b mprep i i')


mkNomRoleEdges :: VertexMap
               -> [(Lemma,Lemma,X'Tree 'PH1,(FNFrame,Range),(FNFrameElement,Maybe EntityInfo),(FNFrameElement,EntityInfo))]
               -> [MGEdge]
mkNomRoleEdges vmap nmatched = do
  let rngidxmap = vmap^.vm_rangeToIndex
  (_lma,_verb,_,(_frm,rng_dp),(subj,mei_subj),(obj,ei_obj)) <- nmatched
  i <- maybeToList (HM.lookup (0,Just rng_dp) rngidxmap)   -- frame
  let rng_obj = ei_obj ^.ei_fullRange
  i' <- maybeToList (HM.lookup (0,Just rng_obj) rngidxmap)  -- frame element
  let lstsubj = case mei_subj of
                  Just ei_subj -> do
                    i'' <- maybeToList (HM.lookup (0,Just (ei_subj^.ei_fullRange)) rngidxmap)  -- frame element
                    [MGEdge subj False Nothing i i'']
                  Nothing -> []
  (MGEdge obj False (ei_obj^.ei_prep) i i') : lstsubj



mkInnerDPEdges :: VertexMap
               -> [(EntityInfo,DPInfo)]
               -> [MGEdge]
mkInnerDPEdges vmap entities = do
    (ei,di) <- entities
    let mrng = Just (ei^.ei_fullRange)
        appos = maybe [] (mkRelEdge "Instance" "Type" mrng) (di^.adi_appos)
        comp = do c <- maybeToList (di^.adi_compof)
                  if (c^.ei_isClause)
                    then mkRelEdge "Means" "Goal" mrng c
                    else mkRelEdge "Subset" "Group" mrng c
        poss = concatMap (mkRelEdge "Possession" "Owner" mrng) (di^.adi_poss)
        adjs = do a <- di^.adi_adjs
                  p <- maybeToList (a^.ei_prep)
                  f <- if (a^.ei_isTime)
                         then [("Time_vector","Event","Landmark_event")]
                         else maybeToList (ppRelFrame p)
                  mkRelEdge (f^._2) (f^._3) mrng a
    (appos ++ comp ++ poss ++ adjs)
  where
    rngidxmap = vmap^.vm_rangeToIndex
    mkRelEdge role1 role2 mrng ei = do
      let rng' = ei^.ei_fullRange
          mprep = ei^.ei_prep
      i_frame <- maybeToList (HM.lookup (1,Just rng') rngidxmap)
      i_1 <- maybeToList (HM.lookup (0,mrng) rngidxmap)
      i_2 <- maybeToList (HM.lookup (0,Just rng') rngidxmap)
      [MGEdge role1 True Nothing i_frame i_1, MGEdge role2 False mprep i_frame i_2]


mkPrepEdges :: VertexMap
            -> [((Int,FNFrame,Text,[(FNFrameElement,(Bool,Range))]),MGVertex)]
            -> [MGEdge]
mkPrepEdges vmap ientities2 = do
  let rngidxmap = vmap^.vm_rangeToIndex
  (i_frame,_frm,_prep,felst) <- map fst ientities2
  (fe,(b,rng)) <- felst
  i_elem <- maybeToList (HM.lookup (0,Just rng) rngidxmap)
  [MGEdge fe b Nothing i_frame i_elem]


mkCorefEdges :: VertexMap
             -> [(EntityInfo,DPInfo)]
             -> [MGEdge]
mkCorefEdges vmap entities = do
  let rngidxmap = vmap^.vm_rangeToIndex
  (_,di) <- entities
  (rng0,rng1) <- maybeToList (di^.adi_coref)
  i_0 <- maybeToList (HM.lookup (0,Just rng0) rngidxmap)
  i_1 <- maybeToList (HM.lookup (0,Just rng1) rngidxmap)
  [MGEdge "ref" False Nothing i_0 i_1]




mkMGEdges :: VertexMap
          -> ([(Range,VerbProperty (Zipper '[Lemma]),X'Tree 'PH1,FrameMatchResult,Maybe (SenseID,Bool))]
             ,[(Lemma,Lemma,X'Tree 'PH1,(FNFrame,Range),(FNFrameElement,Maybe EntityInfo),(FNFrameElement,EntityInfo))]
             )
          -> ([(EntityInfo,DPInfo)]
             ,[((Int,FNFrame,Text,[(FNFrameElement,(Bool,Range))]),MGVertex)])
          -> [MGEdge]
mkMGEdges vmap (matched,nmatched) (entities1_0,ientities2) =
  let edges0 = mkRoleEdges vmap matched
      edges01= mkNomRoleEdges vmap nmatched
      edges1 = mkInnerDPEdges vmap entities1_0
      edges2 = mkPrepEdges vmap ientities2
      edges3 = mkCorefEdges vmap entities1_0
      edges = edges0 ++ edges01 ++ edges1 ++ edges2 ++ edges3
  in  {- trace ("\nEDGES\n" ++ intercalate "\n" (map show edges)) $ -} edges



meaningGraph :: AnalyzePredata -> SentStructure -> MeaningGraph
meaningGraph apredata sstr =
  let lst_x'trvstrcp = mkTriples sstr
      tagged = sstr^.ss_tagged
      matched = mapMaybe (\(x'tr,vstr,cp) -> matchFrame (apredata^.analyze_framedb) x'tr (vstr,cp)) lst_x'trvstrcp
      depmap = do x'tr <- sstr^.ss_x'trs
                  dependencyOfX'Tree x'tr
      --
      dps = do x'tr <- sstr^.ss_x'trs
               (_,DPCase dp) <- biList x'tr
               return (x'tr,dp)
      nmatched = mapMaybe (\(x'tr,dp) -> matchNomFrame apredata x'tr tagged dp) dps
      --
      (vertices,entities1_0,ientities2,headfull) = mkMGVertices (tagged,depmap) (matched,nmatched)
      --
      rangeid :: MGVertex -> (Int,Maybe Range)
      rangeid mv = (if mv^?_MGPredicate._4._PredAppos == Just () then 1 else 0, mv^.mv_range)
      --
      rngidxmap = HM.fromList [(rangeid v, v^.mv_id) | v <- vertices ]
      -- hnrngidxmap = HM.fromList [(rng, v^.mv_id) | v <- vertices, rng <- v^.._MGEntity._3._Just ]
      vmap = VertexMap rngidxmap {- hnrngidxmap -} depmap headfull
      edges = mkMGEdges vmap (matched,nmatched) (entities1_0,ientities2)
  in MeaningGraph vertices edges


isEntity :: MGVertex -> Bool
isEntity x = case x of
               MGEntity {..} -> True
               _             -> False


tagMG :: MeaningGraph -> [(Range,Text)] -> MeaningGraph
tagMG mg wikilst =
  let addtag x@MGEntity {..} = case _mv_head_range of
                                 Nothing -> x
                                 Just rng -> if (rng `elemRevIsInsideR` map fst wikilst) && isEntity x
                                             then x { _mv_resolved_entities = map (^. _2) (filter (\w -> (w^._1) `isInsideR` rng) wikilst)}
                                             else x
      addtag x = x
      mg' = mg ^.. mg_vertices . traverse . to addtag
  in MeaningGraph mg' (mg ^. mg_edges)


changeMGText :: MeaningGraph -> MeaningGraph
changeMGText mg =
  let mg' = mg ^.. mg_edges
                 . traverse
                 . to (\x -> x & (me_relation .~ (FNFrameElement . T.replace "&" "-AND-" . unFNFrameElement) (x ^. me_relation)))
      mg'' = mg ^.. mg_vertices
                  . traverse
                  . to (\x -> case x of
                           MGEntity {..} -> x { _mv_text = T.replace "&" "-AND-" _mv_text }
                           MGPredicate {..} -> x
                       )
  in MeaningGraph mg'' mg'
