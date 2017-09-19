{-# LANGUAGE OverloadedStrings  #-}

module SRL.Statistics where

import           Control.Lens      ((^.),(^..),to,traverse)
import           Data.Graph
import           Data.Maybe        (mapMaybe)
import           Data.Tree
--
import           SRL.Analyze.Type  (MGVertex(..),SentStructure(..),MeaningGraph
                                   ,me_start,me_end,mg_edges,mg_vertices,mv_id)


-- DocStructure mtokenss sentitems mergedtags sstrs

getGraphFromMG :: MeaningGraph -> Maybe Graph
getGraphFromMG mg =
  let vtxs = mg ^. mg_vertices ^.. traverse . mv_id  
      edgs = mg ^. mg_edges ^.. traverse . to (\x -> (x ^. me_start, x ^. me_end))
  in case vtxs of
    [] -> Nothing
    v  -> let bounds = (minimum v, maximum v)
              graph  = buildG bounds edgs
          in (Just graph)

numberOfPredicate :: SentStructure -> Int
numberOfPredicate (SentStructure _ _ _ _ _ _ vstrs) = length vstrs

numberOfMGVerbPredicate :: MeaningGraph -> Int
numberOfMGVerbPredicate mg = length $ mapMaybe fmtVerb (mg ^. mg_vertices)
  where
    fmtVerb (MGEntity    _ _ _  _ )  = Nothing
    fmtVerb (MGPredicate i _ _f _v) = Just i
    fmtVerb (MGNominalPredicate i _ _) = Nothing

numberOfMGPredicate :: MeaningGraph -> Int
numberOfMGPredicate mg = length $ mapMaybe fmtVerb (mg ^. mg_vertices)
  where
    fmtVerb (MGEntity    _ _ _  _ )  = Nothing
    fmtVerb (MGPredicate i _ _f _v) = Just i
    fmtVerb (MGNominalPredicate i _ _) = Just i 

-- I change the name to farthest, not furthest
farthestPath :: Graph -> Int
farthestPath grph = maximum $ map (length . flatten) $ dff grph


numberOfIsland :: Graph -> Int
numberOfIsland grph = length $ components $ grph
