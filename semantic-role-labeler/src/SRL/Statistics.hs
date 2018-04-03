{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module SRL.Statistics where

import           Control.Lens        ((^.),(^..),(^?),to,traverse,_4)
-- import           Control.Lens.Extras (is)
-- import           Control.Monad       (guard)
import           Data.Graph
import           Data.Maybe          (mapMaybe)
--
import           SRL.Analyze.Type    (SentStructure(..),MeaningGraph
                                     ,_MGPredicate,_PredVerb
                                     ,me_start,me_end,me_ismodifier
                                     ,mg_edges,mg_vertices,mv_id
                                     ,ss_verbStructures
                                     )


-- DocStructure mtokenss sentitems mergedtags sstrs

getGraphFromMG :: MeaningGraph -> Maybe Graph
getGraphFromMG mg =
  let vtxs = mg ^.. mg_vertices . traverse . mv_id
      edgs = mg ^.. mg_edges . traverse . to (\x -> if x^.me_ismodifier then (x^.me_end,x^.me_start) else (x ^. me_start, x ^. me_end))
  in case vtxs of
    [] -> Nothing
    v  -> let bounds = (minimum v, maximum v)
              graph  = buildG bounds edgs
          in (Just graph)


numberOfPredicate :: SentStructure -> Int
numberOfPredicate ss = length (ss^.ss_verbStructures)


numberOfMGVerbPredicate :: MeaningGraph -> Int
numberOfMGVerbPredicate mg = length $ mapMaybe fmtVerb (mg ^. mg_vertices)
  where
    fmtVerb mv = mv^?_MGPredicate._4._PredVerb >> return (mv^.mv_id)


numberOfMGPredicate :: MeaningGraph -> Int
numberOfMGPredicate mg = length $ mapMaybe fmtVerb (mg ^. mg_vertices)
  where
    fmtVerb mv = mv^?_MGPredicate >> return (mv^.mv_id)
