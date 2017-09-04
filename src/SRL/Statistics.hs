{-# LANGUAGE OverloadedStrings  #-}

module SRL.Statistics where

import           Control.Lens      ((^.),(^..),_1,_2,_3,_4,_5,to,traverse)
import           Data.Graph
import           Data.Maybe        (fromMaybe,mapMaybe)
import           Data.Monoid       ((<>))
import           Data.Tree
--
import           NLP.Syntax.Format (formatAspect,formatTense)
import           SRL.Analyze.Type  (MGVertex(..),SentStructure(..)
                                   ,me_start,me_end
                                   ,mg_edges,mg_vertices,mv_id)


-- DocStructure mtokenss sentitems mergedtags sstrs

getGraphFromMG mg =
  let vertices = mg ^. mg_vertices ^.. traverse . mv_id  
      edges    = mg ^. mg_edges ^.. traverse . to (\x -> (x ^. me_start, x ^. me_end))
  in case vertices of
    [] -> Nothing
    v  -> let bounds = (minimum v, maximum v)
              graph  = buildG bounds edges
          in (Just graph)

numberOfPredicate (SentStructure i ptr vps clausetr mcpstr vstrs) = length vstrs

numberOfMGPredicate mg = length $ mapMaybe fmtVerb (mg ^. mg_vertices)
  where
    fmtVerb (MGEntity    _ _ _  ) = Nothing
    fmtVerb (MGPredicate i _ f v) = Just i

furthestPath grph = maximum $ map (length . flatten) $ dff grph

testEdges = [(2,3)]
testBounds = (1,2)

testGraph = buildG testBounds testEdges
