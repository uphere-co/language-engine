{-# LANGUAGE OverloadedStrings #-}

module Test.GraphOps where

import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import           Assert                                (assert,massertEqual,eassertEqual)
import           Data.Int                              (Int32, Int64)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           Control.Monad.ST                      (runST)

import qualified Data.Vector.Algorithms.Search as VS
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector                   as V

import           WikiEL.BinarySearch                   (binarySearchLR,binarySearchLRBy)



orderingByFrom,orderingByTo :: Ord a => (a,a) -> (a,a) -> Ordering
orderingByFrom (lf,lt) (rf,rt) = compare lf rf
orderingByTo   (lf,lt) (rf,rt) = compare lt rt

neighbor :: (UV.Unbox a, Ord a) => UV.Vector (a,a) -> VS.Comparison (a,a) -> a -> UV.Vector (a,a)
neighbor edges comp node = f sorted comp node
  where
    sorted = UV.modify (sortBy comp) edges
    f es comp node= runST $ do
      mvec <- UV.unsafeThaw es
      (beg, end) <- binarySearchLRBy comp mvec (node,node)
      return (UV.slice beg (end-beg) es)

isIn :: (UV.Unbox a, Ord a) => UV.Vector a -> a -> Bool
isIn vals = f sorted
  where
    sorted = UV.modify sort vals
    f vs x = runST $ do
      mvec <- UV.unsafeThaw vs
      (beg,end) <- binarySearchLR mvec x
      --return (UV.slice beg (end-beg) vs)
      return ((end-beg)/=0)

unique :: (UV.Unbox a, Ord a) => UV.Vector a -> UV.Vector a
unique vs = UV.foldl' f UV.empty sorted
  where
    sorted = UV.modify sort vs
    f accum v | UV.null accum = UV.singleton v
    f accum v | UV.last accum == v = accum
    f accum v = UV.snoc accum v

type Dist = Int32
accumReachable :: (UV.Unbox a, Ord a) => UV.Vector (a,Dist) -> Dist -> (a -> UV.Vector (a,a)) -> (UV.Vector a, Dist) ->  UV.Vector (a,Dist)
accumReachable accum cutoff dfn (frontiers,dist) | cutoff==dist = accum
accumReachable accum cutoff dfn (frontiers,dist) = accumReachable (UV.concat [ns, accum]) cutoff dfn (nexts, dist+1)
  where
    f dfn node = UV.map snd (dfn node)
    reachable = UV.concatMap (f dfn) frontiers
    news = UV.filter (not . isIn (UV.map fst accum)) reachable
    nexts = unique news
    ns = UV.map (\x -> (x, dist+1)) nexts

nodesForward :: (UV.Unbox a, Ord a) => UV.Vector (a,a) -> a -> Dist -> UV.Vector (a,Dist)
nodesForward dEdges node cutoff = accumReachable accum cutoff dForwardEdges (UV.fromList [node],0)
  where
    accum = UV.fromList [(node,0)]
    dForwardEdges = neighbor dEdges orderingByFrom

accumPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> UV.Vector a -> V.Vector (UV.Vector a)
accumPaths dfn path = UV.foldl' f accum (UV.map snd (dfn from))
  where
    from = UV.last path
    accum = V.empty
    f accum to = V.snoc accum (UV.snoc path to)   

allPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> a -> Dist -> V.Vector (UV.Vector a)
allPaths dfn node cutoff = f 0 (V.singleton (UV.singleton node))
  where
    f dist paths | dist==cutoff = paths
    f dist paths = f (dist+1) (V.concatMap (accumPaths dfn) paths)

newtype NodeID = NodeID Int64
             deriving (Show,Ord,Eq)
type Edge = (NodeID, NodeID)

vectorizeEdges :: [Edge] -> UV.Vector (Int64, Int64)
vectorizeEdges edges = UV.fromList (map (\(NodeID from, NodeID to) -> (from,to)) edges)

testNeighborNodes :: TestTree
testNeighborNodes = testCaseSteps "Get neighbor nodes in directed/undirected graph" $ \step -> do
  let
    e from to = (NodeID from, NodeID to)
    directed = vectorizeEdges ([ e 1 2, e 1 3, e 2 4, e 3 4, e 4 1
                               , e 1 5, e 5 6, e 5 7, e 1 6
                               , e 9 1, e 8 1, e 10 9,e 10 8,e 10 1
                               , e 8 11,e 11 3
                               ] :: [Edge])
    undirected = UV.concatMap (\(x,y) -> UV.fromList [(x,y),(y,x)]) directed

    dForwardEdges  = neighbor directed orderingByFrom
    dBackwardEdges = neighbor directed orderingByTo
    uForwardEdges  = neighbor undirected orderingByFrom
    uBackwardEdges = neighbor undirected orderingByTo

  eassertEqual (dForwardEdges  1) (UV.fromList [(1,2),(1,3),(1,5),(1,6)])
  eassertEqual (dBackwardEdges 1) (UV.fromList [(4,1),(9,1),(8,1),(10,1)])
  eassertEqual (uBackwardEdges 4) (UV.fromList [(3,4),(2,4),(1,4)])
  eassertEqual (uForwardEdges  4) (UV.fromList [(4,2),(4,1),(4,3)])

  eassertEqual (nodesForward directed 10 2) (UV.fromList [(2,2),(3,2),(5,2),(6,2),(11,2),(1,1),(8,1),(9,1),(10,0)])
  eassertEqual (nodesForward directed 10 3) (UV.fromList [(4,3),(7,3),(2,2),(3,2),(5,2),(6,2),(11,2),(1,1),(8,1),(9,1),(10,0)])
  eassertEqual (nodesForward directed 3  3) (UV.fromList [(2,3),(5,3),(6,3),(1,2),(4,1),(3,0)])

testAllPaths :: TestTree
testAllPaths = testCaseSteps "Get all paths within distance cutoff between a pair of nodes" $ \step -> do
  let
    e from to = (NodeID from, NodeID to)
    directed = vectorizeEdges ([ e 1 2, e 1 3, e 2 4, e 3 4, e 4 1
                               , e 1 5, e 5 6, e 5 7, e 1 6
                               , e 9 1, e 8 1, e 10 9,e 10 8,e 10 1
                               , e 8 11,e 11 3
                               ] :: [Edge])
    dForwardEdges  = neighbor directed orderingByFrom

  print $ accumPaths dForwardEdges (UV.fromList [10,8])
  let
    paths = accumPaths dForwardEdges (UV.fromList [10])
  print paths
  print $ V.concatMap (accumPaths dForwardEdges) paths
  print $ allPaths dForwardEdges 10 2
  print $ allPaths dForwardEdges 10 3


testUtilsForShortedPath :: TestTree
testUtilsForShortedPath = testCaseSteps "Test helper functions for shorted path" $ \step -> do
  let
    nodes = UV.fromList ([1,3,6,2,9,7] :: [Int32])
  assert (UV.all (isIn nodes) nodes)
  eassertEqual (UV.filter (isIn nodes) (UV.fromList [1,2,3,4,5])) (UV.fromList [1,2,3])
  assert (not (isIn nodes 5))
  let
    aa = [(1,0),(3,0),(4,0),(5,0)] :: [(Int32,Int32)]
    bb = [(1,0),(3,0)] :: [(Int32,Int32)]    
  eassertEqual (UV.filter (isIn nodes . fst) (UV.fromList aa)) (UV.fromList bb)
  let
    vs = UV.fromList ([6,1,11,3,1,2,6,2,9,7] :: [Int32])
    uvs = UV.fromList ([1,2,3,6,7,9,11] :: [Int32])    
  eassertEqual (unique vs) uvs


allTest :: TestTree
allTest =
  testGroup
    "All graph operation unit tests"
    [ testNeighborNodes
    , testUtilsForShortedPath
    , testAllPaths
    ]
