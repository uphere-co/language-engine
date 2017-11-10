{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze.Match.MeaningGraph where

import           Control.Lens
import           Control.Monad                (join)
import           Data.Function                (on)
import qualified Data.HashMap.Strict    as HM
import           Data.List                    (find,groupBy,sortBy)
import           Data.Maybe                   (catMaybes,fromMaybe,isJust,isNothing,listToMaybe,mapMaybe,maybeToList)
import qualified Data.Text              as T
import           Data.Text                    (Text)
--
import           Data.Bitree                  (getRoot1)
import           Data.BitreeZipper            (current,extractZipperById)
import           Data.Range                   (Range,elemRevIsInsideR,isInsideR)
import           Lexicon.Type
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util              (GetIntLemma(..),isLemmaAs,intLemma0)
import           NLP.Type.PennTreebankII
--
import           SRL.Analyze.Match.Entity
import           SRL.Analyze.Match.Frame
import           SRL.Analyze.Type             (MGVertex(..),MGEdge(..),MeaningGraph(..)
                                              ,SentStructure,VerbStructure
                                              ,PredicateInfo(..)
                                              ,_PredNoun,_MGPredicate
                                              ,ss_x'tr,ss_tagged,ss_verbStructures
                                              ,vs_roleTopPatts,vs_vp
                                              ,me_relation,mv_range,mv_id,mg_vertices,mg_edges)

dependencyOfX'Tree :: X'Tree p -> [(Range,Range)]
dependencyOfX'Tree (PN (rng0,_) xs) = map ((rng0,) . fst . getRoot1) xs ++ concatMap dependencyOfX'Tree xs
dependencyOfX'Tree (PL _)           = []


mkEntityFun (rng,txt,DI mrngtxt_appos _ mrngtxt_part) =
  let mkRel frm mrngtxt' = flip (maybe []) mrngtxt' $ \(rng',txt') -> [ \i'  -> MGEntity i' (Just rng') txt' []
                                                                      , \i'' -> MGPredicate i'' (Just rng') frm PredNoun ]
  in (\i -> MGEntity i rng txt []) : (mkRel "Instance" mrngtxt_appos ++ mkRel "Partitive" mrngtxt_part)


mkMGVertices tagged x'tr matched =
  let preds = flip map matched $ \(rng,vprop,frame,sense,_mselected,_) i
                                   -> MGPredicate i (Just rng) frame (PredVerb sense (simplifyVProp vprop))
      ipreds = zipWith ($) preds [1..]

      entities0 = do (_,_,_,_,mselected,_) <- matched
                     (_,felst) <- maybeToList mselected
                     (_fe,x) <- felst
                     case x of
                       CompVP_Unresolved _ -> []
                       CompVP_CP _cp -> [] -- CP is not an entity.
                       CompVP_DP dp -> return (entityFromDP x'tr tagged dp)
                       CompVP_PP pp -> maybeToList (entityFromDP x'tr tagged <$> (pp^?complement._CompPP_DP))

      filterFrame = filter (\(rng,_,_) -> not (any (\p -> p^.mv_range == rng) ipreds))
      --

      entities1_0 = filterFrame
                  . map head
                  . groupBy ((==) `on` (^._1))
                  . sortBy (compare `on` (^._1))
                  $ entities0


      entities1 = concatMap mkEntityFun entities1_0

      entities2 = do (_,_,_,_,_,lst) <- matched
                     (frm,prep,felst) <- lst
                     return (\i -> ((i,frm,prep,felst),MGPredicate i Nothing frm (PredPrep prep)))
      n_ipreds = length ipreds
      n_entities1 = length entities1
      n_entities2 = length entities2
      ientities1 = zipWith ($) entities1 (enumFrom (n_ipreds+1))
      ientities2 = zipWith ($) entities2 (enumFrom (n_ipreds+n_entities1+1))
      vertices = ipreds ++ ientities1 ++ (map snd ientities2)

  in (vertices,entities1_0,ientities2)


mkRoleEdges (rngidxmap,depmap) matched = do
  (rng,_,_,_,mselected,_) <- matched
  i <- maybeToList (HM.lookup (0,Just rng) rngidxmap)   -- frame
  (_,felst) <- maybeToList mselected
  (fe,x) <- felst
  (rng',mprep) <- case x of
                    CompVP_Unresolved _ -> []
                    CompVP_CP cp -> let z_cp = cp^.maximalProjection
                                        mprep = case cp^.headX of
                                                  C_PHI -> Nothing
                                                  C_WORD z -> (z^?to current.to intLemma0._Just._2.to unLemma)
                                                                 >>= \prep -> if prep == "that" then Nothing else return prep
                                    in return (getRange (current z_cp),mprep)
                    CompVP_DP dp -> return (fromMaybe (dp^.maximalProjection) (dp^?complement._Just.headX),Nothing)   -- for the time being
                    CompVP_PP pp -> return (pp^.complement.to compPPToRange,pp^?headX.hp_prep._Prep_WORD)
  i' <- maybeToList (HM.lookup (0,Just rng') rngidxmap)  -- frame element
  let b = isJust (find (== (rng',rng)) depmap)
  return (MGEdge fe b mprep i i')


mkInnerDPEdges rngidxmap entities = do
    (mrng,_,di) <- entities
    -- let appos = di^.adi_appos
    (mkRelEdge "Instance" "Type" (mrng,di^.adi_appos) ++ mkRelEdge "Subset" "Group" (mrng,di^.adi_partitive))
  where
    mkRelEdge role1 role2 (mrng,mrngtxt') = do
      (rng',_) <- maybeToList mrngtxt'
      i_frame <- maybeToList (HM.lookup (1,Just rng') rngidxmap)
      i_1 <- maybeToList (HM.lookup (0,mrng) rngidxmap)
      i_2 <- maybeToList (HM.lookup (0,Just rng') rngidxmap)
      [MGEdge role1 True Nothing i_frame i_1, MGEdge role2 False Nothing i_frame i_2]


mkPrepEdges rngidxmap ientities2 = do
  (i_frame,frm,prep,felst) <- map fst ientities2
  (fe,(b,rng)) <- felst
  i_elem <- maybeToList (HM.lookup (0,Just rng) rngidxmap)
  [MGEdge fe b Nothing i_frame i_elem]


mkCorefEdges rngidxmap entities1_0 = do
  (mrng,_,DI _ mrng' _) <- entities1_0
  rng' <- maybeToList mrng'
  i_0 <- maybeToList (HM.lookup (0,mrng) rngidxmap)
  i_1 <- maybeToList (HM.lookup (0,Just rng') rngidxmap)
  [MGEdge "ref" False Nothing i_0 i_1]


mkMGEdges (rngidxmap,depmap) matched (entities1_0,ientities2) =
  let edges0 = mkRoleEdges (rngidxmap,depmap) matched
      edges1 = mkInnerDPEdges rngidxmap entities1_0
      edges2 = mkPrepEdges rngidxmap ientities2
      edges3 = mkCorefEdges rngidxmap entities1_0
  in edges0 ++ edges1 ++ edges2 ++ edges3


meaningGraph :: SentStructure -> MeaningGraph
meaningGraph sstr =
  let (x'tr,lst_vstrcp) = mkTriples sstr
      tagged = sstr^.ss_tagged
      matched = mapMaybe matchFrame lst_vstrcp
      depmap = dependencyOfX'Tree =<< x'tr
      --
      (vertices,entities1_0,ientities2) = mkMGVertices tagged x'tr matched
      --
      rangeid :: MGVertex -> (Int,Maybe Range)
      rangeid mv = (if mv^?_MGPredicate._4._PredNoun == Just () then 1 else 0, mv^.mv_range)
      --
      rngidxmap = HM.fromList [(rangeid v, v^.mv_id) | v <- vertices ]
      edges = mkMGEdges (rngidxmap,depmap) matched (entities1_0,ientities2)
  in MeaningGraph vertices edges


isEntity :: MGVertex -> Bool
isEntity x = case x of
               MGEntity {..} -> True
               _             -> False


tagMG :: MeaningGraph -> [(Range,Text)] -> MeaningGraph
tagMG mg wikilst =
  let mg' = mg ^.. mg_vertices
                 . traverse
                 . to (\x -> case x^.mv_range of
                               Nothing -> x
                               Just rng -> if (rng `elemRevIsInsideR` map fst wikilst) && isEntity x
                                           then x { _mv_resolved_entities = map (^. _2) (filter (\w -> (w^._1) `isInsideR` rng) wikilst)}
                                           else x
                      )
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
