{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module SRL.Analyze.ARB where

import           Control.Lens              ((^.),(^..),_2,_3,to)
import           Control.Monad.Loops       (unfoldM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (State,runState,execState,evalState,get,put,modify')
import           Data.Array                ((!))
import           Data.Graph                (Graph,Vertex,topSort)
import           Data.List                 (delete,find,elem)
import           Data.Maybe                (catMaybes,fromJust,isNothing,mapMaybe,maybeToList)
import           Data.Text                 (Text)
import qualified Data.Tree as Tr
--
import           NLP.Syntax.Clause         (hoistMaybe) -- this should be moved somewhere
import           NLP.Syntax.Type.Verb      (vp_lemma)
import           NLP.Type.PennTreebankII   (Lemma(..))
import           NLP.Shared.Type
import           SRL.Analyze.Type
import           SRL.Statistics
--
import           Debug.Trace


type VertexARB = (Vertex, Vertex, [Vertex])


isFrame :: MGVertex -> Bool
isFrame (MGEntity {..})           = False
isFrame (MGPredicate {..})        = True
isFrame (MGNominalPredicate {..}) = True


isEntity :: MGVertex -> Bool
isEntity = (not . isFrame)

{-
cnvtVtxToMGV :: MeaningGraph -> Vertex -> Maybe MGVertex 
cnvtVtxToMGV mg vtx =
  let vrtcs = (mg ^. mg_vertices)
  in find (\x -> (x ^. mv_id) == vtx) vrtcs

cnvtEdgToMGE :: MeaningGraph -> Edge -> Maybe MGEdge
cnvtEdgToMGE mg edg =
  let edgs = (mg ^. mg_edges)
  in find (\x -> (x ^. me_start) == (fst edg) && (x ^. me_end) == (snd edg)) edgs



    
findFrame :: [MGVertex] -> Int -> Maybe Text
findFrame mvs i =
  let mr = find (\mv -> (mv ^. mv_id) == i) mvs
  in case mr of
    Nothing                            -> Nothing
    Just (v@(MGPredicate {..}))        -> Just (v ^. mv_frame)
    Just (v@(MGNominalPredicate {..})) -> Just (v ^. mv_frame)
    Just _                             -> Nothing



attached :: Graph -> Vertex -> [Vertex]
attached grph vtx =
  let lnodes = concat $ fmap Tr.levels $ dfs grph [vtx]
      mlnode = drop 1 lnodes
  in case mlnode of
    []    -> []
    lnode -> head lnode
-}

subjectList = [ "Agent","Speaker","Owner","Cognizer","Actor","Author","Cognizer_agent"
              , "Protagonist", "Cause"
              ]
isSubject t = any (\a -> t == a) subjectList

findRel :: [MGEdge] -> Int -> Int -> Maybe MGEdge
findRel mes i j = find (\me -> (me ^. me_start) == i && (me ^. me_end) == j) mes

findVertex :: [MGVertex] -> Int -> Maybe MGVertex
findVertex mvs i = find (\mv -> (mv ^. mv_id) == i) mvs

findLabel :: [MGVertex] -> Int -> Maybe Text
findLabel mvs i = do
  v <- findVertex mvs i
  case v of
    MGEntity {..}           -> Just (v ^. mv_text)
    MGPredicate {..}        -> Just (v ^. mv_verb . vp_lemma . to unLemma)
    MGNominalPredicate {..} -> Just (v ^. mv_frame)


findSubjectObjects :: MeaningGraph
                   -> Graph
                   -> Vertex
                   -> MaybeT (State [Vertex]) ARB
findSubjectObjects mg grph frmid = do
  let children = grph ! frmid
  v <- hoistMaybe $ findVertex (mg^.mg_vertices) frmid
  frmtxt <- case v of
              MGEntity           {..} -> hoistMaybe Nothing
              MGPredicate        {..} -> return (v^.mv_frame)
              MGNominalPredicate {..} -> return (v^.mv_frame)
  verbtxt <- hoistMaybe $ findLabel (mg^.mg_vertices) frmid 
  let rels = mapMaybe (findRel (mg^.mg_edges) frmid) children
  (sidx,subject) <- hoistMaybe $ do
                      e <- find (\e -> isSubject (e^.me_relation)) rels
                      let sidx = e^.me_end
                      (sidx,) . (e^.me_relation,) <$> findLabel (mg^.mg_vertices) sidx
  objs <- lift $ do
    fmap catMaybes . flip mapM (filter (\e -> e ^.me_end /= sidx) rels) $ \o -> do
      let oidx = o^.me_end
      runMaybeT $ do
        v <- hoistMaybe (findVertex (mg^.mg_vertices) oidx)
        if isFrame v
          then do
            lift (modify' (delete oidx))
            arb <- findSubjectObjects mg grph oidx
            return (Left arb)
          else do
            olabel <- hoistMaybe (findLabel (mg^.mg_vertices) oidx)
            return (Right (o^.me_relation,olabel))
  return (ARB frmtxt subject (frmtxt,verbtxt) objs [])


mkARB1 :: (MeaningGraph,Graph) -> State [Vertex] (Maybe (Maybe ARB))
mkARB1 (mg,graph) = do
  xs <- get
  case xs of
    [] -> return Nothing
    (y:ys) -> do
      put ys
      r <- runMaybeT $ findSubjectObjects mg graph y
      return (Just r)


mkARB :: MeaningGraph -> [ARB]
mkARB mg = catMaybes $ do
  let mgraph = getGraphFromMG mg
  graph <- maybeToList mgraph 
  let framelst = map (^.mv_id) $ filter isFrame $ mg^. mg_vertices

      vs = filter (`elem` framelst) $ topSort graph
  evalState (unfoldM (mkARB1 (mg,graph))) vs


  -- trace (show graph ++ ":" ++ show s) []
  -- trace (show graph ++ ":" ++ show vs) [] 
{-   in case mgraph of
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
-}
