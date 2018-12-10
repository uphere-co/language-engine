{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze.Match.MeaningGraph where

import           Control.Applicative          ((<|>))
import           Control.Lens
import           Control.Lens.Extras          (is)
import           Data.Bifoldable              (biList)
import           Data.Function                (on)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (find,groupBy,sortBy)
import           Data.Maybe                   (fromMaybe,mapMaybe,maybeToList)
import           Data.Monoid                  ((<>))
import qualified Data.Text              as T
import           Data.Text                    (Text)
------ other language-engine
import           Data.Bitree                  ( getRoot1 )
import           Data.Range                   ( Range, elemRevIsInsideR, isInsideR )
import           Lexicon.Type
import           NLP.Syntax.Type.Resolve      ( Resolved(..), Referent(..)
                                              , referent2CompVP
                                              , referent2Trace
                                              )
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Type.PennTreebankII
----- semantic-role-labeler
import           SRL.Analyze.Match.Entity
import           SRL.Analyze.Match.Frame
import           SRL.Analyze.Match.Preposition (ppRelFrame)
import           SRL.Analyze.Type             ( MGVertex(..)
                                              , MGEdge(..)
                                              , MeaningGraph(..)
                                              , PredicateInfo(..)
                                              , SentStructure
                                              , SRLData
                                              , VertexID(..)
                                              , VertexMap(..)
                                              , vm_rangeToIndex
                                              , vm_rangeDependency
                                              , vm_headRangeToFullRange
                                              , srldata_framedb
                                              , isEntity
                                              , ss_tagged
                                              , ss_x'trs
                                              , me_relation
                                              , mv_range
                                              , mv_id
                                              , mg_vertices
                                              , mg_edges
                                              , toReg
                                              , vidToRange
                                              )
import           SRL.Analyze.Type.Match       (DPInfo(..),EmptyCategoryIndex(..),EntityInfo(..),FrameMatchResult(..)
                                              ,RangePair(..)
                                              ,adi_appos,adi_compof,adi_coref,adi_poss,adi_adjs
                                              ,ei_eci,ei_rangePair,ei_prep,ei_isClause,ei_isTime
                                              ,rp_full,rp_head
                                              ,emptyDPInfo
                                              )




dependencyOfX'Tree :: X'Tree 'PH1 -> [(Range,Range)]
dependencyOfX'Tree (PN (rng0,l) xs) =
  let deps = map ((rng0,) . fst . getRoot1) xs ++ concatMap dependencyOfX'Tree xs
  in case l of
       CPCase cp -> fromMaybe deps $ do
                      rng_wh <- cp^?specifier._Just.coidx_content._SpecCP_WH
                      return ((rng_wh,rng0):deps)
       _ -> deps


dependencyOfX'Tree (PL _)           = []



mkEntityFun :: (EntityInfo,DPInfo) -> [(Int -> MGVertex)]
mkEntityFun (EI t (RangePair rng rnghead) _mprep txt _ _,di) =
  let mkECIVertex Nothing            = toReg rng
      mkECIVertex (Just ECI_NULL)    = toReg rng
      mkECIVertex (Just (ECI_PRO j)) = VertexPRO j
      mkRel frm (EI _eci (RangePair rng' rng'') _ txt' False _) = [ \i'  -> MGEntity i' (toReg rng') (Just rng'') txt' []
                                                                  , \i'' -> MGPredicate i'' (InnerDPRange rng') frm PredAppos ]
      mkRel frm (EI _eci (RangePair rng' _) _ _ True _)  = [ \i'' -> MGPredicate i'' (InnerDPRange rng') frm PredAppos ]

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
  in [ \i -> MGEntity i (mkECIVertex t) (Just rnghead) (case t of Just (ECI_PRO j) -> "PRO_" <> T.pack (show j); _ -> txt) [] ]
     ++ appos ++ comp ++ poss ++ adjs



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
  let preds = flip map matched $ \(rng,vprop,_,FMR idiom frm _ _,sense) i
                                   -> MGPredicate i (toReg rng) frm (PredVerb idiom sense (vprop^.vp_index,vprop^.vp_index) (simplifyVProp vprop))
      npreds = flip map nmatched $ \(lma,verb,_,(frm,rng_dp),_,_) ->
                                  \i -> MGPredicate i (toReg rng_dp) frm (PredNominalized lma rng_dp verb)
      ipreds = zipWith ($) (preds ++ npreds) [1..]

      ett_verb :: [(EntityInfo,DPInfo)]
      ett_verb  = do (rng,_,x'tr,FMR _ _ mselected _,_) <- matched
                     (_,felst) <- maybeToList mselected
                     (_fe,x) <- felst
                     case referent2CompVP x of
                       CompVP_CP rng_cp -> do
                         cp <- maybeToList (cpdpppFromX'Tree x'tr rng_cp _CPCase)
                         speccp <- cp^..specifier._Just
                         case speccp^.coidx_content of
                           SpecCP_WH rng_wh -> let headtxt = T.intercalate " " (tokensByRange tagged rng_wh)
                                                   ei = EI Nothing (RangePair rng_wh rng_wh) Nothing headtxt True False
                                               in [(ei,emptyDPInfo)]
                           _ -> []
                       CompVP_AP rng_ap -> do
                         ap <- maybeToList (cpdpppFromX'Tree x'tr rng_ap _APCase)
                         return (entityFromAP tagged ap)
                       CompVP_DP rng_dp -> do
                         case x of
                           RefVariable _ (RFree_WHDP _) -> do
                             let headtxt = T.intercalate " " (tokensByRange tagged rng_dp)
                                 ei = EI Nothing (RangePair rng_dp rng_dp) Nothing headtxt True False
                             [(ei,emptyDPInfo)]
                           _ -> do
                             dp <- maybeToList (cpdpppFromX'Tree x'tr rng_dp _DPCase)
                             let y = entityFromDP x'tr tagged (referent2Trace x,dp)
                             if is _Just (find (== (rng_dp,rng)) depmap) then [] else [y]
                       CompVP_PP rng_pp -> maybeToList $ do
                         pp <- cpdpppFromX'Tree x'tr rng_pp _PPCase
                         rng_dp <- pp^?complement._CompPP_DP
                         dp <- cpdpppFromX'Tree x'tr rng_dp _DPCase
                         return (entityFromDP x'tr tagged (Nothing,dp))

      filterFrame = filter (\(ei,_) -> not (any (\p -> p^.mv_range.to vidToRange == Just (ei^.ei_rangePair.rp_full)) ipreds))
      --

      ett_nominal :: [(EntityInfo,DPInfo)]
      ett_nominal = do (_lma,_verb,_,(_frm,_rng_dp),(_subj,mei_subj),(_obj,ei_obj)) <- nmatched
                       let lstsubj = case mei_subj of
                                       Just ei_subj -> [(ei_subj,DI Nothing Nothing Nothing [] [])]
                                       _ -> []
                       (ei_obj,DI Nothing Nothing Nothing [] []) : lstsubj
      --
      ett_verbnom = filterFrame
                  . map head
                  . groupBy ((==) `on` (\x->(x^._1.ei_eci,x^._1.ei_rangePair.rp_full)))
                  . sortBy (compare `on` (\x->(x^._1.ei_eci,x^._1.ei_rangePair.rp_full)))
                  $ (ett_verb ++ ett_nominal)
      --
      ettfunc_verbnom = concatMap mkEntityFun ett_verbnom
      --
      ettfunc_prep = do (_,_,_,FMR _ _ _ lst,_) <- matched
                        (frm,prep,felst) <- lst
                        return (\i -> ((i,frm,prep,felst),MGPredicate i (VertexPrep i) frm (PredPrep prep)))
      --
      n_ipreds = length ipreds
      n_ettverbnom = length ettfunc_verbnom
      iett_verbnom = zipWith ($) ettfunc_verbnom (enumFrom (n_ipreds+1))
      iett_prep = zipWith ($) ettfunc_prep (enumFrom (n_ipreds+n_ettverbnom+1))
      vertices = ipreds ++ iett_verbnom ++ (map snd iett_prep)
      headfull = flip concatMap ett_verbnom $ \(ei,di) ->
                   let appos = di^..adi_appos._Just
                       compof = di^..adi_compof._Just
                       poss = di^.adi_poss
                       adjs = di^.adi_adjs
                       eis = ei : (appos ++ compof ++ poss ++ adjs)
                   in map (\e -> (e^.ei_rangePair.rp_head,e^.ei_rangePair.rp_full)) eis
  in (vertices,ett_verbnom,iett_prep,headfull)


mkRoleEdges :: VertexMap
            -> [(Range,VerbProperty (Zipper '[Lemma]),X'Tree 'PH1,FrameMatchResult,Maybe (SenseID,Bool))]
            -> [MGEdge]
mkRoleEdges vmap matched = do
  let rngidxmap = vmap^.vm_rangeToIndex
      depmap = vmap^.vm_rangeDependency
      headfull = vmap^.vm_headRangeToFullRange
  (rng,_,x'tr,FMR _ _ mselected _,_) <- matched
  i <- maybeToList (HM.lookup (RegularRange rng) rngidxmap)   -- frame
  (_,felst) <- maybeToList mselected
  (fe,x) <- felst
  (rng',mprep) <- maybeToList $ case referent2CompVP x of
                                  CompVP_CP rng_cp -> do
                                    cp <- cpdpppFromX'Tree x'tr rng_cp _CPCase
                                    let mprep = case cp^.headX of
                                                  C_PHI -> Nothing
                                                  C_WORD prep -> if prep == Lemma "that" then Nothing else return (unLemma prep)
                                    rng' <- (cp^?specifier._Just.coidx_content._SpecCP_WH <|> return rng_cp)
                                    return (rng',mprep)
                                  CompVP_DP rng_dp -> return (rng_dp,Nothing)
                                  CompVP_AP rng_ap -> return (rng_ap,Nothing)
                                  CompVP_PP rng_pp -> do
                                    pp <- cpdpppFromX'Tree x'tr rng_pp _PPCase
                                    return (pp^.complement.to (compPPToRange SPH1),pp^?headX.hp_prep._Prep_WORD)
  let rng'full = fromMaybe rng' (lookup rng' headfull)
  case referent2Trace x of
    Just (PRO,j) -> do
      i' <- maybeToList (HM.lookup (VertexPRO j) rngidxmap)  -- frame element
      i'' <- maybeToList (HM.lookup (RegularRange rng'full) rngidxmap)
      [MGEdge fe False mprep i i', MGEdge "PRO" False Nothing i' i'']

    _            -> do
      i' <- maybeToList (HM.lookup (RegularRange rng'full) rngidxmap)  -- frame element
      let b = is _Just (find (== (rng',rng)) depmap)
      [MGEdge fe b mprep i i']


mkNomRoleEdges :: VertexMap
               -> [(Lemma,Lemma,X'Tree 'PH1,(FNFrame,Range),(FNFrameElement,Maybe EntityInfo),(FNFrameElement,EntityInfo))]
               -> [MGEdge]
mkNomRoleEdges vmap nmatched = do
  let rngidxmap = vmap^.vm_rangeToIndex
  (_lma,_verb,_,(_frm,rng_dp),(subj,mei_subj),(obj,ei_obj)) <- nmatched
  i <- maybeToList (HM.lookup (RegularRange rng_dp) rngidxmap)   -- frame
  let rng_obj = ei_obj^.ei_rangePair.rp_full
  i' <- maybeToList (HM.lookup (RegularRange rng_obj) rngidxmap)  -- frame element
  let lstsubj = case mei_subj of
                  Just ei_subj -> do
                    i'' <- maybeToList (HM.lookup (RegularRange (ei_subj^.ei_rangePair.rp_full)) rngidxmap)  -- frame element
                    [MGEdge subj False Nothing i i'']
                  Nothing -> []
  (MGEdge obj False (ei_obj^.ei_prep) i i') : lstsubj



mkInnerDPEdges :: VertexMap
               -> [(EntityInfo,DPInfo)]
               -> [MGEdge]
mkInnerDPEdges vmap entities = do
    (ei,di) <- entities
    let rng = ei^.ei_rangePair.rp_full
        appos = maybe [] (mkRelEdge "Instance" "Type" rng) (di^.adi_appos)
        comp = do c <- maybeToList (di^.adi_compof)
                  if (c^.ei_isClause)
                    then mkRelEdge "Means" "Goal" rng c
                    else mkRelEdge "Subset" "Group" rng c
        poss = concatMap (mkRelEdge "Possession" "Owner" rng) (di^.adi_poss)
        adjs = do a <- di^.adi_adjs
                  p <- maybeToList (a^.ei_prep)
                  f <- if (a^.ei_isTime)
                         then [("Time_vector","Event","Landmark_event")]
                         else maybeToList (ppRelFrame p)
                  mkRelEdge (f^._2) (f^._3) rng a
    (appos ++ comp ++ poss ++ adjs)
  where
    rngidxmap = vmap^.vm_rangeToIndex
    mkRelEdge role1 role2 rng ei = do
      let rng' = ei^.ei_rangePair.rp_full
          mprep = ei^.ei_prep
      i_frame <- maybeToList (HM.lookup (InnerDPRange rng') rngidxmap)
      i_1 <- maybeToList (HM.lookup (RegularRange rng) rngidxmap)
      i_2 <- maybeToList (HM.lookup (RegularRange rng') rngidxmap)
      [MGEdge role1 True Nothing i_frame i_1, MGEdge role2 False mprep i_frame i_2]


mkPrepEdges :: VertexMap
            -> [((Int,FNFrame,Text,[(FNFrameElement,(Bool,Range))]),MGVertex)]
            -> [MGEdge]
mkPrepEdges vmap ientities2 = do
  let rngidxmap = vmap^.vm_rangeToIndex
  (i_frame,_frm,_prep,felst) <- map fst ientities2
  (fe,(b,rng)) <- felst
  i_elem <- maybeToList (HM.lookup (RegularRange rng) rngidxmap)
  [MGEdge fe b Nothing i_frame i_elem]


mkCorefEdges :: VertexMap
             -> [(EntityInfo,DPInfo)]
             -> [MGEdge]
mkCorefEdges vmap entities = do
  let rngidxmap = vmap^.vm_rangeToIndex
  (_,di) <- entities
  (rng0,rng1) <- maybeToList (di^.adi_coref)
  i_0 <- maybeToList (HM.lookup (RegularRange rng0) rngidxmap)
  i_1 <- maybeToList (HM.lookup (RegularRange rng1) rngidxmap)
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
  in edges0 ++ edges01 ++ edges1 ++ edges2 ++ edges3


meaningGraph :: SRLData -> SentStructure -> MeaningGraph
meaningGraph sdata sstr =
  let lst_x'trvstrcp = mkTriples sstr
      tagged = sstr^.ss_tagged
      matched =
        mapMaybe
          (\(x'tr,vstr,cp) -> matchFrame (sdata^.srldata_framedb) x'tr (vstr,cp))
          lst_x'trvstrcp
      depmap = do x'tr <- sstr^.ss_x'trs
                  dependencyOfX'Tree x'tr
      --
      dps = do x'tr <- sstr^.ss_x'trs
               (_,DPCase dp) <- biList x'tr
               return (x'tr,dp)
      nmatched = mapMaybe (\(x'tr,dp) -> matchNomFrame sdata x'tr tagged dp) dps
      --
      (vertices,entities1_0,ientities2,headfull) =
        mkMGVertices (tagged,depmap) (matched,nmatched)
      --
      rngidxmap =
        HM.fromList [(v^.mv_range, v^.mv_id) | v <- vertices ]
      vmap = VertexMap rngidxmap depmap headfull
      edges = mkMGEdges vmap (matched,nmatched) (entities1_0,ientities2)
  in MeaningGraph vertices edges




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
