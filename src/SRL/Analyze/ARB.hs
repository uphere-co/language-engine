{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module SRL.Analyze.ARB where

import           Control.Applicative       ((<|>))
import           Control.Lens
import           Control.Lens.Extras
import           Data.List
import           Control.Monad.Loops       (unfoldM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (State,evalState,get,put,modify')
import           Data.Array                ((!))
import           Data.Graph                (Graph,Vertex,topSort)
import           Data.List                 (delete,find,elem)
import           Data.Maybe                (catMaybes,fromMaybe
                                           ,mapMaybe,maybeToList)
import           Data.Text                 (Text)
--
import           Lexicon.Mapping.Causation (causeDualMap,cm_causativeFrame,cm_externalAgent)
import           Lexicon.Type              (FNFrame(..),FNFrameElement(..),RoleInstance,SenseID)
import           NLP.Syntax.Clause         (hoistMaybe) -- this should be moved somewhere
import           NLP.Syntax.Type.Verb      (vp_lemma,vp_negation)
import           NLP.Type.PennTreebankII   (Lemma(..))
import           NLP.Shared.Type
import           SRL.Analyze.Type
import           SRL.Statistics
--


type VertexARB = (Vertex, Vertex, [Vertex])

squashRelFrame :: MeaningGraph -> MeaningGraph
squashRelFrame mg0 = last (mg0 : unfoldr f mg0)
  where f mg = do
         let vs = mg^.mg_vertices
             es = mg^.mg_edges
         v <- find (is _Nothing . (^.mv_range)) vs
         let i = v^.mv_id
             vs' = filter ((/= i) . (^.mv_id)) vs


         j0 <- (^.me_end) <$> find (\e -> (e^.me_start) == i && (e^.me_ismodifier)) es
         j1 <- (^.me_end) <$> find (\e -> (e^.me_start) == i && not (e^.me_ismodifier)) es
         (_,_,frm,info) <- v ^? _MGPredicate
         prep <- info^?_PredPrep
         let e' = MGEdge (FNFrameElement (unFNFrame frm)) False (Just prep) j0 j1
             es' = filter ((/= i) . (^.me_start)) es ++ [e']
             mg' = mg & (mg_vertices .~ vs') . (mg_edges .~ es')

         return (mg',mg')


isFrame :: MGVertex -> Bool
isFrame (MGEntity {..})           = False
isFrame (MGPredicate {..})        = True


isEntity :: MGVertex -> Bool
isEntity = (not . isFrame)


subjectList :: [FNFrameElement]
subjectList = [ "Agent","Speaker","Owner","Cognizer","Actor","Author","Cognizer_agent"
              , "Protagonist", "Cause"
              ]


isSubject :: [RoleInstance] -> FNFrame -> Maybe (SenseID,Bool) -> FNFrameElement -> Bool
isSubject _       _     Nothing rel = any (==rel) subjectList
isSubject rolemap frame (Just (sense,cause)) rel =
  fromMaybe False $ do
    roles <- lookup sense rolemap
    if cause
      then do
        m <- find (\m -> m^.cm_causativeFrame == frame) causeDualMap
        return (m^.cm_externalAgent == rel)
      else
        (((==unFNFrameElement rel) <$> lookup "arg0" roles)
         <|>
        ((==unFNFrameElement rel) <$> lookup "arg1" roles))


findRel :: [MGEdge] -> Int -> Int -> Maybe MGEdge
findRel mes i j = find (\me -> (me ^. me_start) == i && (me ^. me_end) == j) mes


findVertex :: [MGVertex] -> Int -> Maybe MGVertex
findVertex mvs i = find (\mv -> (mv ^. mv_id) == i) mvs


findLabel :: [MGVertex] -> Int -> Maybe Text
findLabel mvs i = do
  v <- findVertex mvs i
  case v of
    MGEntity {..}           -> Just _mv_text
    MGPredicate {..}        -> case _mv_pred_info of
                                 PredVerb _ vrb -> Just (vrb ^. vp_lemma . to unLemma)
                                 PredPrep p     -> Just p
                                 PredNominalized n _ -> Just (unLemma n)
                                 PredAppos      -> Just (unFNFrame _mv_frame)


findSubjectObjects :: ([RoleInstance],MeaningGraph,Graph)
                   -> Vertex
                   -> MaybeT (State [Vertex]) ARB
findSubjectObjects (rolemap,mg,grph) frmid = do
  let chldrn = grph ! frmid
  v <- hoistMaybe $ findVertex (mg^.mg_vertices) frmid
  (frmtxt,msense,mneg) <- case v of
                            MGEntity           {..} -> hoistMaybe Nothing
                            MGPredicate        {..} ->
                              case _mv_pred_info of
                                PredVerb sns vrb -> return (unFNFrame _mv_frame,sns,vrb^.vp_negation)
                                PredPrep _       -> return (unFNFrame _mv_frame,Nothing,Nothing)
                                PredNominalized _ _ -> return (unFNFrame _mv_frame,Nothing,Nothing)
                                PredAppos        -> return (unFNFrame _mv_frame,Nothing,Nothing)
  verbtxt <- hoistMaybe $ findLabel (mg^.mg_vertices) frmid
  let rels = mapMaybe (findRel (mg^.mg_edges) frmid) chldrn
  (sidx,subject) <- hoistMaybe $ do
                      e <- find (\e -> isSubject rolemap (FNFrame frmtxt) msense (e^.me_relation)) rels
                      let sidx = e^.me_end
                      (sidx,) . (unFNFrameElement (e^.me_relation),) <$> findLabel (mg^.mg_vertices) sidx
  (objs :: [(FrameElement,Either (PrepOr ARB) (PrepOr Text))]) <- lift $ do
    fmap catMaybes . flip mapM (filter (\e -> e ^.me_end /= sidx) rels) $ \o -> do
      let oidx = o^.me_end
          oprep = o^.me_prep
          orole = unFNFrameElement (o^.me_relation)
      runMaybeT $ do
        v' <- hoistMaybe (findVertex (mg^.mg_vertices) oidx)
        if isFrame v'
          then do
            lift (modify' (delete oidx))
            arb <- findSubjectObjects (rolemap,mg,grph) oidx
            return (orole,Left (PrepOr oprep arb))
          else do
            olabel <- hoistMaybe (findLabel (mg^.mg_vertices) oidx)
            return (orole,Right (PrepOr oprep olabel))
  return (ARB frmtxt subject (frmtxt,verbtxt) (maybe False (const True) mneg) objs [])


mkARB1 :: ([RoleInstance],MeaningGraph,Graph) -> State [Vertex] (Maybe (Maybe ARB))
mkARB1 (rolemap,mg,graph) = do
  xs <- get
  case xs of
    [] -> return Nothing
    (y:ys) -> do
      put ys
      r <- runMaybeT $ findSubjectObjects (rolemap,mg,graph) y
      return (Just r)


mkARB :: [RoleInstance] -> MeaningGraph -> [ARB]
mkARB rolemap mg0 = catMaybes $ do
  let mg = squashRelFrame mg0
      mgraph = getGraphFromMG mg
  graph <- maybeToList mgraph
  let framelst = map (^.mv_id) $ filter isFrame $ mg^. mg_vertices
      vs = filter (`elem` framelst) $ topSort graph
  evalState (unfoldM (mkARB1 (rolemap,mg,graph))) vs
