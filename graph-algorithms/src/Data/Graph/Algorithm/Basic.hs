module Data.Graph.Algorithm.Basic where

import           Data.Graph
import           Data.Tree


maxConnectedNodes :: Graph -> Int
maxConnectedNodes grph = maximum $ map (length . flatten) $ dff grph

numberOfIsland :: Graph -> Int
numberOfIsland grph = length $ components $ grph
