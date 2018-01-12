{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module SRL.Analyze.MeaningTree where

import           Control.Applicative       ((<|>))
import           Control.Lens
import           Control.Monad             (guard)
import qualified Data.Text             as T
import           Control.Monad             (mzero)
import           Control.Monad.Loops       (unfoldM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (State,evalState,get,put,modify')
import           Data.Graph                (Graph,Vertex,topSort)
import           Data.List                 (delete,find,elem)
import           Data.Maybe                (catMaybes,fromMaybe,listToMaybe
                                           ,mapMaybe,maybeToList)
import           Data.Range                (Range)
import           Data.Text                 (Text)
--
import           Lexicon.Mapping.Causation (causeDualMap,cm_causativeFrame,cm_externalAgent)
import           Lexicon.Type              (FNFrame(..),FNFrameElement(..),RoleInstance,SenseID)
import           NLP.Semantics.Type        (MeaningRoleContent(..),MeaningRole(..),MeaningTree(..)
                                           ,PrepOr(..))
import           NLP.Syntax.Clause         (hoistMaybe) -- this should be moved somewhere
import           NLP.Syntax.Type.Verb      (vp_negation)
import           NLP.Type.PennTreebankII   (Lemma(..))
import           SRL.Analyze.Type
import           SRL.Statistics
--


-- type VertexTriple = (Vertex, Vertex, [Vertex])
{-
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
-}

isFrame :: MGVertex -> Bool
isFrame (MGEntity {..})           = False
isFrame (MGPredicate {..})        = True


isEntity :: MGVertex -> Bool
isEntity = (not . isFrame)


subjectList :: [FNFrameElement]
subjectList = [ "Agent","Speaker","Owner","Cognizer","Actor","Author","Cognizer_agent"
              , "Protagonist", "Cause", "Instance", "Figure"
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


forwardLinks :: [MGEdge] -> Int -> [Int]
forwardLinks mes i = [me^.me_end| me <- filter (\me -> (me ^. me_start) == i) mes]


backwardLinks :: [MGEdge] -> Int -> [Int]
backwardLinks mes i = [me^.me_start| me <- filter (\me -> (me ^. me_end) == i && me^.me_ismodifier) mes]




findVertex :: [MGVertex] -> Int -> Maybe MGVertex
findVertex mvs i = find (\mv -> (mv ^. mv_id) == i) mvs


findLabel :: [MGVertex] -> Int -> Maybe (Text,Maybe Range)
findLabel mvs i = do
  v <- findVertex mvs i
  case v of
    MGEntity {..}           -> Just (_mv_text,_mv_head_range)
    MGPredicate {..}        -> case _mv_pred_info of
                                 PredVerb idiom _ rng _vrb -> Just (T.intercalate " " idiom,Just rng)
                                 PredPrep p           -> Just (p,Nothing)
                                 PredNominalized n rng _  -> Just (unLemma n,Just rng)
                                 PredAppos            -> Just ("",Nothing)

constructMeaningRole :: ([RoleInstance],MeaningGraph,Graph)
                     -> MGEdge
                     -> State [Vertex] (Maybe MeaningRole)
constructMeaningRole (rolemap,mg,grph) o = do
  let oidx = o^.me_end
      oprep = o^.me_prep
      orole = unFNFrameElement (o^.me_relation)
  runMaybeT $ do
    v' <- hoistMaybe (findVertex (mg^.mg_vertices) oidx)
    let isM = o^.me_ismodifier
        isF = isFrame v'
        blnks = backwardLinks (mg^.mg_edges) oidx
        mlnk = (False,) <$> listToMaybe (forwardLinks (mg^.mg_edges) oidx)
    if isM
    then return (MeaningRole oidx orole (PrepOr oprep (Terminal ("<t>",Nothing)(Just (True,v'^.mv_id)))))
    else if isF
         then do
            lift (modify' (delete oidx))
            sub <- constructMeaningTree (rolemap,mg,grph) oidx
            return (MeaningRole oidx orole (PrepOr oprep (SubFrame sub)))
         else do
            olabel <- hoistMaybe (findLabel (mg^.mg_vertices) oidx)
            case blnks of
              [] -> return (MeaningRole oidx orole (PrepOr oprep (Terminal olabel mlnk)))
              _ -> do
                subs <- flip mapM blnks $ \blnk -> do
                  v'' <- hoistMaybe (findVertex (mg^.mg_vertices) blnk)
                  guard (isFrame v'')
                  lift (modify' (delete blnk))
                  constructMeaningTree (rolemap,mg,grph) blnk
                return (MeaningRole oidx orole (PrepOr oprep (Modifier olabel subs)))


constructMeaningTree :: ([RoleInstance],MeaningGraph,Graph)
                     -> Vertex
                     -> MaybeT (State [Vertex]) MeaningTree
constructMeaningTree (rolemap,mg,grph) frmid = do
  let -- chldrn = grph ! frmid
      chldrn0 = forwardLinks (mg^.mg_edges) frmid
  v <- hoistMaybe $ findVertex (mg^.mg_vertices) frmid
  (vid,frmtxt,_msense,mneg) <-
    case v of
      MGEntity    {..} -> mzero
      MGPredicate {..} ->
        let vid = v^.mv_id
        in case _mv_pred_info of
             PredVerb _ sns _ vrb  -> return (vid,unFNFrame _mv_frame,sns,vrb^.vp_negation)
             PredPrep _            -> return (vid,unFNFrame _mv_frame,Nothing,Nothing)
             PredNominalized _ _ _ -> return (vid,unFNFrame _mv_frame,Nothing,Nothing)
             PredAppos             -> return (vid,unFNFrame _mv_frame,Nothing,Nothing)
  verbtxt <- hoistMaybe $ findLabel (mg^.mg_vertices) frmid

  let rels = mapMaybe (findRel (mg^.mg_edges) frmid) chldrn0
  arguments :: [MeaningRole] <- lift (catMaybes <$> mapM (constructMeaningRole (rolemap,mg,grph)) rels)
  let subs = backwardLinks (mg^.mg_edges) frmid
  subordinates <- mapM (constructMeaningTree (rolemap,mg,grph)) subs
  lift (modify' (delete frmid))
  -- trace ("\nconstructMeaningTree:" ++ show (verbtxt,frmid,subs)) $
  return (MeaningTree vid frmtxt verbtxt (maybe False (const True) mneg) arguments subordinates)


mkMeaningTree1 :: ([RoleInstance],MeaningGraph,Graph) -> State [Vertex] (Maybe MeaningTree)
mkMeaningTree1 (rolemap,mg,graph) = do
  xs <- get
  case xs of
    [] -> return Nothing
    (y:ys) -> do
      put ys
      runMaybeT $ constructMeaningTree (rolemap,mg,graph) y


mkMeaningTree :: [RoleInstance] -> MeaningGraph -> [MeaningTree]
mkMeaningTree rolemap mg = do
  let mgraph = getGraphFromMG mg
  graph <- maybeToList mgraph
  let framelst = map (^.mv_id) $ filter isFrame $ mg^. mg_vertices
      vs = filter (`elem` framelst) $ topSort graph
  -- trace ("\vertices\n" ++ intercalate "\n" (map show (mg^.mg_vertices)) ) $ return ()      
  -- trace ("\nedges\n" ++ intercalate "\n" (map show (mg^.mg_edges)) ) $ return ()
  evalState (unfoldM (mkMeaningTree1 (rolemap,mg,graph))) vs
