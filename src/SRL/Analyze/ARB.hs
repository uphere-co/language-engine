{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module SRL.Analyze.ARB where

import           Control.Applicative       ((<|>))
import           Control.Lens              ((^.),(^..),_2,_3,to)
import           Control.Monad             (join)
import           Control.Monad.Loops       (unfoldM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (State,runState,execState,evalState,get,put,modify')
import           Data.Array                ((!))
import           Data.Graph                (Graph,Vertex,topSort)
import           Data.List                 (delete,find,elem)
import           Data.Maybe                (catMaybes,fromMaybe
                                           ,mapMaybe,maybeToList)
import           Data.Text                 (Text)
import qualified Data.Tree as Tr
--
import           Lexicon.Mapping.Causation (causeDualMap,cm_causativeFrame,cm_externalAgent)
import           Lexicon.Type              (RoleInstance)
import           NLP.Syntax.Clause         (hoistMaybe) -- this should be moved somewhere
import           NLP.Syntax.Type.Verb      (vp_lemma,vp_negation)
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



subjectList = [ "Agent","Speaker","Owner","Cognizer","Actor","Author","Cognizer_agent"
              , "Protagonist", "Cause"
              ]
-- isSubject t = 


isSubject rolemap frame Nothing rel = any (==rel) subjectList
isSubject rolemap frame (Just (sense,cause)) rel =
  fromMaybe False $ do
    roles <- lookup sense rolemap
    if cause
      then do
        m <- find (\m -> m^.cm_causativeFrame == frame) causeDualMap
        return (m^.cm_externalAgent == rel)
      else 
        (((==rel) <$> lookup "arg0" roles)
         <|>
        ((==rel) <$> lookup "arg1" roles))


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


findSubjectObjects :: ([RoleInstance],MeaningGraph,Graph)
                   -> Vertex
                   -> MaybeT (State [Vertex]) ARB
findSubjectObjects (rolemap,mg,grph) frmid = do
  let children = grph ! frmid
  v <- hoistMaybe $ findVertex (mg^.mg_vertices) frmid
  (frmtxt,msense,mneg) <- case v of
                            MGEntity           {..} -> hoistMaybe Nothing
                            MGPredicate        {..} -> return (_mv_frame,Just _mv_sense,_mv_verb^.vp_negation)
                            MGNominalPredicate {..} -> return (_mv_frame,Nothing,Nothing)
  verbtxt <- hoistMaybe $ findLabel (mg^.mg_vertices) frmid 
  let rels = mapMaybe (findRel (mg^.mg_edges) frmid) children
  (sidx,subject) <- hoistMaybe $ do
                      e <- find (\e -> isSubject rolemap frmtxt msense (e^.me_relation)) rels
                      let sidx = e^.me_end
                      (sidx,) . (e^.me_relation,) <$> findLabel (mg^.mg_vertices) sidx
  (objs :: [(Text,Either (PrepOr ARB) (PrepOr Text))]) <- lift $ do
    fmap catMaybes . flip mapM (filter (\e -> e ^.me_end /= sidx) rels) $ \o -> do
      let oidx = o^.me_end
          oprep = o^.me_prep
          orole = o^.me_relation
      runMaybeT $ do
        v <- hoistMaybe (findVertex (mg^.mg_vertices) oidx)
        if isFrame v
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
mkARB rolemap mg = catMaybes $ do
  let mgraph = getGraphFromMG mg
  graph <- maybeToList mgraph 
  let framelst = map (^.mv_id) $ filter isFrame $ mg^. mg_vertices
      vs = filter (`elem` framelst) $ topSort graph
  evalState (unfoldM (mkARB1 (rolemap,mg,graph))) vs



