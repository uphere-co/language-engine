{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SRL.Analyze.ARB where

import           Control.Lens     ((^.),(^..),_2,_3,to)
import           Data.Graph
import           Data.List        (find)
import           Data.Maybe       (catMaybes,fromJust,isNothing,mapMaybe)
import           Data.Text        (Text)
import qualified Data.Tree as Tr
--
import           NLP.Syntax.Type.Verb    (vp_lemma)
import           NLP.Type.PennTreebankII (Lemma(..))
import           NLP.Shared.Type
import           SRL.Analyze.Type
import           SRL.Statistics
--
import           Debug.Trace

isMGPredicate :: MGVertex -> Bool
isMGPredicate (MGEntity {..}) = False
isMGPredicate (MGPredicate {..}) = True

isMGEntity :: MGVertex -> Bool
isMGEntity = (not . isMGPredicate)

cnvtVtxToMGV :: MeaningGraph -> Vertex -> Maybe MGVertex 
cnvtVtxToMGV mg vtx =
  let vrtcs = (mg ^. mg_vertices)
      mv = find (\x -> (x ^. mv_id) == vtx) vrtcs
  in mv

cnvtEdgToMGE :: MeaningGraph -> Edge -> Maybe MGEdge
cnvtEdgToMGE mg edg =
  let edgs = (mg ^. mg_edges)
      me = find (\x -> (x ^. me_start) == (fst edg) && (x ^. me_end) == (snd edg)) edgs
  in me

findRel :: [MGEdge] -> Int -> Int -> Maybe Text
findRel mes i j =
  let mr = find (\me -> (me ^. me_start) == i && (me ^. me_end) == j) mes
  in case mr of
    Nothing -> Nothing
    Just e  -> Just (e ^. me_relation)

findLabel :: [MGVertex] -> Int -> Maybe (Text,Text)
findLabel mvs i =
  let mr = find (\mv -> (mv ^. mv_id) == i) mvs
  in case mr of
    Nothing                            -> Nothing
    Just (v@(MGEntity {..}))           -> Just ("", v ^. mv_text)
    Just (v@(MGPredicate {..}))        -> Just ("", v ^. mv_verb . vp_lemma . to unLemma)
    Just (v@(MGNominalPredicate {..})) -> Just ("", v ^. mv_frame)
    
findFrame :: [MGVertex] -> Int -> Maybe Text
findFrame mvs i =
  let mr = find (\mv -> (mv ^. mv_id) == i) mvs
  in case mr of
    Nothing                            -> Nothing
    Just (v@(MGPredicate {..}))        -> Just (v ^. mv_frame)
    Just (v@(MGNominalPredicate {..})) -> Just (v ^. mv_frame)
    Just _                             -> Nothing


type VertexARB = (Vertex, Vertex, [Vertex])

{- 
findAgent :: MeaningGraph -> Graph -> Vertex -> Maybe Edge
findAgent mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> let children = attached grph vtx
                   rels = catMaybes $ map (\n -> (,,) <$> findRel (mg ^. mg_edges) vtx n <*> Just vtx <*> Just n) children
                   agent' = find (\(t,_i,_j) -> t == "Agent") rels
                   agent = fmap (\x -> (x ^. _2, x ^. _3)) agent'
               in agent


findAgentThemes :: MeaningGraph -> Graph -> Vertex -> Maybe VertexARB
findAgentThemes mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> let children = attached grph vtx
                   rels = catMaybes $ map (\n -> (,,) <$> findRel (mg ^. mg_edges) vtx n <*> Just vtx <*> Just n) children
                   agent = fmap (^. _3) $ find (\(t,_i,_j) -> t == "Agent") rels
                   theme1 = fmap (^. _3) $ find (\(t,_i,_j) -> t == "Theme") rels
               in ((,,) <$> agent <*> Just vtx <*> (sequence [theme1]))
-}

subjectList = ["Agent","Speaker","Owner","Cognizer","Actor","Author"]
isSubject t = any (\a -> t == a) subjectList

findSubjectObjects :: MeaningGraph -> Graph -> Vertex -> Maybe VertexARB
findSubjectObjects mg grph vtx = case (cnvtVtxToMGV mg vtx) of
  Nothing -> Nothing
  Just mv -> case (isMGPredicate mv) of
    False   -> Nothing
    True    -> let children = attached grph vtx
                   rels = catMaybes $ map (\n -> (,,) <$> findRel (mg ^. mg_edges) vtx n <*> Just vtx <*> Just n) children
                   subject = fmap (^. _3) $ find (\(t,_i,_j) -> isSubject t) rels
                   objects = map (\x -> Just (x ^. _3)) $ filter (\(t,i,j) -> if (isNothing $ cnvtVtxToMGV mg j) then False else (if (isMGEntity $ fromJust $ cnvtVtxToMGV mg j) then (if (isSubject t) then False else True) else False)) rels
               in ((,,) <$> subject <*> Just vtx <*> (sequence objects))

attached :: Graph -> Vertex -> [Vertex]
attached grph vtx =
  let lnodes = concat $ fmap Tr.levels $ dfs grph [vtx]
      mlnode = drop 1 lnodes
  in case mlnode of
    []    -> []
    lnode -> head lnode

mkARB :: MeaningGraph -> [ARB]
mkARB mg =
  let mgraph = getGraphFromMG mg
  in case mgraph of
    Nothing    -> []
    Just graph ->
      let mgpred = filter isMGPredicate (mg ^. mg_vertices)
          mgpredvtxs = (mgpred ^.. traverse . mv_id)
          agents = mapMaybe (\vtx -> findSubjectObjects mg graph vtx) mgpredvtxs
          vrtcs = mg ^. mg_vertices
          man :: [(Text,(Text,Text),(Text,Text),[Either ARB (Text,Text)])] 
          man = mapMaybe (\(v1,v2,vs) -> (,,,) <$> findFrame vrtcs v2 <*> findLabel vrtcs v1 <*> findLabel vrtcs v2 <*> pure (mapMaybe (\v3 -> fmap Right (findLabel vrtcs v3)) vs)) agents
          arbs = map (\(a,b,c,ds) -> ARB a b c ds []) man
      in trace ("trace : " ++ show mgpredvtxs ++ "  " ++ (show man) ++ "  " ++ (show agents)) arbs

