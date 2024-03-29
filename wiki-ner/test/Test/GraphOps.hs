{-# LANGUAGE OverloadedStrings #-}

module Test.GraphOps where

import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import           Assert                                (assert,massertEqual,eassertEqual)
import           Data.Int                              (Int32, Int64)
import           Control.Arrow                         ((***))

import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector.Generic           as GV
import qualified Data.Vector.Fusion.Bundle     as B

import           WikiEL.Graph


newtype NodeID = NodeID Int64
               deriving (Show,Ord,Eq)
type Edge = (NodeID, NodeID)

vectorizeEdges :: [Edge] -> UV.Vector (Int64, Int64)
vectorizeEdges edges = UV.fromList (map (\(NodeID from, NodeID to) -> (from,to)) edges)


e :: Int64 -> Int64 -> Edge
e from to = (NodeID from, NodeID to)

directed = vectorizeEdges ( [ e 1 2, e 1 3, e 2 4, e 3 4, e 4 1
                            , e 1 5, e 5 6, e 5 7, e 1 6
                            , e 9 1, e 8 1, e 10 9,e 10 8,e 10 1
                            , e 8 11,e 11 3
                            ] :: [Edge])

undirected = UV.concatMap (\(x,y) -> UV.fromList [(x,y),(y,x)]) directed

directed2 = vectorizeEdges ([ e 1 9, e 9 10, e 2 11
                            , e 1 3, e 2 3, e 3 12
                            , e 1 4, e 4 5, e 2 5, e 1 5                            
                            , e 1 6, e 6 7, e 8 7, e 2 8
                            , e 1 13, e 13 14, e 2 14, e 2 13
                            ] :: [Edge])
 
testNeighborNodes :: TestTree
testNeighborNodes = testCaseSteps "Get neighbor nodes in directed/undirected graph" $ \step -> do
  let
    dForwardEdges  = neighbor (sortEdges From directed)
    dBackwardEdges = neighbor (sortEdges To   directed)
    uForwardEdges  = neighbor (sortEdges From undirected)
    uBackwardEdges = neighbor (sortEdges To   undirected)

  eassertEqual (dForwardEdges  1) (UV.fromList [2,3,5,6])
  eassertEqual (dBackwardEdges 1) (UV.fromList [4,9,8,10])
  eassertEqual (uBackwardEdges 4) (UV.fromList [3,2,1])
  eassertEqual (uForwardEdges  4) (UV.fromList [2,1,3])

  eassertEqual (nodesForward directed 10 2) (UV.fromList [(2,2),(3,2),(5,2),(6,2),(11,2),(1,1),(8,1),(9,1),(10,0)])
  eassertEqual (nodesForward directed 10 3) (UV.fromList [(4,3),(7,3),(2,2),(3,2),(5,2),(6,2),(11,2),(1,1),(8,1),(9,1),(10,0)])
  eassertEqual (nodesForward directed 3  3) (UV.fromList [(2,3),(5,3),(6,3),(1,2),(4,1),(3,0)])
  eassertEqual (nodesBackward directed 10 5) (UV.fromList [(10,0)])
  eassertEqual (nodesBackward directed 1 1) (UV.fromList [(4,1),(8,1),(9,1),(10,1),(1,0)])
  eassertEqual (nodesBackward directed 1 3) (UV.fromList [(11,3),(2,2),(3,2),(4,1),(8,1),(9,1),(10,1),(1,0)])

  eassertEqual (nodesBackward undirected 8 1) (UV.fromList [(1,1),(10,1),(11,1),(8,0)])
  eassertEqual (nodesBackward undirected 8 2) (nodesForward undirected 8 2)
  eassertEqual (nodesBackward undirected 8 2) (UV.fromList [(2,2),(3,2),(4,2),(5,2),(6,2),(9,2),(1,1),(10,1),(11,1),(8,0)])
  eassertEqual (nodesBackward undirected 1 1) (UV.fromList [(2,1),(3,1),(4,1),(5,1),(6,1),(8,1),(9,1),(10,1),(1,0)])
  
  eassertEqual (neighborOverlap (nodesForward directed 3 2) (nodesForward directed 10 2)) [((3,0),(3,2)),((1,2),(1,1))]
  eassertEqual (neighborOverlap (nodesBackward directed 7 1) (nodesForward directed 10 2)) [((5,1),(5,2))]
  


testAllPaths :: TestTree
testAllPaths = testCaseSteps "Get all paths within distance cutoff between a pair of nodes" $ \step -> do
  let   
    dForwardEdges  = neighbor (sortEdges From directed)
    u = map UV.fromList :: [[Int64]] -> [UV.Vector Int64]

  eassertEqual (u [[8,11],[8,1]])  (accumPaths dForwardEdges (UV.fromList [8]))
  eassertEqual (u [[8,8,11],[8,8,1]]) (accumPaths dForwardEdges (UV.fromList [8,8])) --Nonsense input. Just for testing
  eassertEqual (u [[10,1],[10,8],[10,9]]) (accumPaths dForwardEdges (UV.fromList [10]))
  eassertEqual (allPathsOf dForwardEdges 10 3)  (u [[10,1,5,7],[10,1,5,6],[10,1,3,4],[10,1,2,4],[10,8,11,3],[10,8,1,6],[10,8,1,5],[10,8,1,3],[10,8,1,2],[10,9,1,6],[10,9,1,5],[10,9,1,3],[10,9,1,2]])
  eassertEqual (allPathsUpto dForwardEdges 10 3) (u [[10],[10,1],[10,8],[10,9],[10,1,6],[10,1,5],[10,1,3],[10,1,2],[10,8,11],[10,8,1],[10,9,1],[10,1,5,7],[10,1,5,6],[10,1,3,4],[10,1,2,4],[10,8,11,3],[10,8,1,6],[10,8,1,5],[10,8,1,3],[10,8,1,2],[10,9,1,6],[10,9,1,5],[10,9,1,3],[10,9,1,2]])
  
  print $ allPathsUpto dForwardEdges 1 5


testPathsOverlap :: TestTree
testPathsOverlap = testCaseSteps "Overlap between a two set of paths" $ \step -> do
  step "Case1"
  let   
    dForwardEdges  = neighbor (sortEdges From directed)
    u = map UV.fromList :: [[Int64]] -> [UV.Vector Int64]
    uu = map (UV.fromList *** UV.fromList) :: [([Int64],[Int64])] -> [(UV.Vector Int64, UV.Vector Int64)]
    x1 = destOverlap (allPathsUpto dForwardEdges 10 1) (allPathsUpto dForwardEdges 4 1)
    x2 = destOverlap (allPathsUpto dForwardEdges 10 2) (allPathsUpto dForwardEdges 4 2)
  
  eassertEqual x1 (zip (u [[10,1]]) (u [[4,1]]))
  eassertEqual x2 (zip (u [[10,1,6],[10,1,5],[10,1,3],[10,1,2],[10,1]]) (u [[4,1,6],[4,1,5],[4,1,3],[4,1,2],[4,1]]))
  eassertEqual (destOverlapUpto dForwardEdges 0 9 8) []
  eassertEqual (destOverlapUpto dForwardEdges 1 9 8) (zip (u [[9,1]]) (u [[8,1]]))
  step "Case2"
  let   
    dfe2  = neighbor (sortEdges From directed2)
  eassertEqual (destOverlapUpto dfe2 0 1 2) []
  eassertEqual (destOverlapUpto dfe2 1 1 3) (uu [([1,3],[3])])
  eassertEqual (destOverlapUpto dfe2 3 1 7) (uu [([1,6,7],[7])])
  eassertEqual (destOverlapUpto dfe2 3 1 5) (uu [([1,4,5],[5]),([1,5],[5])])
  eassertEqual (destOverlapUpto dfe2 3 1 2) (uu [([1,6,7],[2,8,7]),([1,4,5],[2,5]),([1,13],[2,13]),([1,5],[2,5]),([1,3],[2,3])])
  eassertEqual (destOverlapUpto dfe2 1 1 2) (uu [([1,13],[2,13]),([1,5],[2,5]),([1,3],[2,3])])



testUtilsForShortestPath :: TestTree
testUtilsForShortestPath = testCaseSteps "Test helper functions for shorted path" $ \step -> do
  step "IsIn, uniqueness"
  let
    uv = UV.fromList :: [Int32] -> UV.Vector Int32
    nodes  = uv [1,3,6,2,9,7]
  assert (UV.all (isIn nodes) nodes)
  eassertEqual (UV.filter (isIn nodes) (uv [1,2,3,4,5])) (uv [1,2,3])
  assert (not (isIn nodes 5))

  let
    aa = [(1,0),(3,0),(4,0),(5,0)] :: [(Int32,Int32)]
    bb = [(1,0),(3,0)] :: [(Int32,Int32)]    
  eassertEqual (UV.filter (isIn nodes . fst) (UV.fromList aa)) (UV.fromList bb)
  let
    vs  = uv [6,1,11,3,1,2,6,2,9,7]
    uvs = uv [1,2,3,6,7,9,11]
  eassertEqual (unique vs) uvs

  step "Set operations"
  let
    vs1 = uv [1,3,6,2,9,7] 
    vs2 = uv [1,11,11,6,9,5,7]
  eassertEqual (intersection [vs1, vs2]) (uv [1,6,9,7])
  eassertEqual (union [vs1, vs2]) (uv [1,3,6,2,9,7,5,11,11])
  

allTest :: TestTree
allTest =
  testGroup
    "All graph operation unit tests"
    [ testNeighborNodes
    , testUtilsForShortestPath
    , testAllPaths
    , testPathsOverlap
    ]
