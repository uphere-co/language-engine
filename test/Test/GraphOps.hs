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
import qualified Data.Vector.Generic           as GV
import qualified Data.Vector                   as V
import qualified Data.Vector.Fusion.Bundle     as B

import           WikiEL.BinarySearch                   (binarySearchLR,binarySearchLRBy)


isIn :: (UV.Unbox a, Ord a) => UV.Vector a -> a -> Bool
isIn vals = f sorted
  where
    sorted = UV.modify sort vals
    f vs x = runST $ do
      mvec <- UV.unsafeThaw vs
      (beg,end) <- binarySearchLR mvec x
      return ((end-beg)/=0)

-- NOTE: ordering is not preserved.
unique :: (UV.Unbox a, Ord a) => UV.Vector a -> UV.Vector a
unique vs = UV.foldl' f UV.empty sorted
  where
    sorted = UV.modify sort vs
    f accum v | UV.null accum = UV.singleton v
    f accum v | UV.last accum == v = accum
    f accum v = UV.snoc accum v

type Dist = Int32



from,to :: (a,a) -> a
from (x,_) = x
to   (_,x) = x

edgeOrdering :: Ord a => ((a,a)->a) -> (a,a) -> (a,a) -> Ordering
edgeOrdering direction left right = compare (direction left) (direction right)
orderingByFrom,orderingByTo :: Ord a => (a,a) -> (a,a) -> Ordering
orderingByFrom = edgeOrdering from
orderingByTo   = edgeOrdering to

{-
  returns edges inward/outward the input node, depends on `comp` operator
  comp : defines ordering between edges. If it orders by from side of edges,
         `neighbor` returns edges starting from the input node.
-}
neighbor :: (UV.Unbox a, Ord a) => UV.Vector (a,a) -> ((a,a)->a) -> a -> UV.Vector (a,a)
neighbor edges direction = f sorted comp
  where
    comp = edgeOrdering direction
    sorted = UV.modify (sortBy comp) edges
    f es comp node= runST $ do
      mvec <- UV.unsafeThaw es
      (beg, end) <- binarySearchLRBy comp mvec (node,node)
      return (UV.slice beg (end-beg) es)

{-
fn :: a -> UV.Vector (a,a)
In this module, `fn` arguments denote partialy evaluated `neighbor` functions. 
It returns edges connedted to the input node of type `a`.
Direction of the edge is determined by the `comp` argument of the `neighbor`
-}

accumReachable :: (UV.Unbox a, Ord a) => ((a,a)->a) -> UV.Vector (a,Dist) -> Dist -> (a -> UV.Vector (a,a)) -> (UV.Vector a, Dist) ->  UV.Vector (a,Dist)
accumReachable fDestNode accum cutoff fn (frontiers,dist) | cutoff==dist = accum
accumReachable fDestNode accum cutoff fn (frontiers,dist) = accumReachable fDestNode (UV.concat [ns, accum]) cutoff fn (nexts, dist+1)
  where
    f fn node = UV.map fDestNode (fn node) -- get nodes, instead of edges
    reachable = UV.concatMap (f fn) frontiers -- edges connedted to the frontier nodes
    news = UV.filter (not . isIn (UV.map fst accum)) reachable -- `reachable` edges that are not already visited
    nexts = unique news -- remove duplicated edges in `news`
    ns = UV.map (\x -> (x, dist+1)) nexts -- new frontier nodes

-- returns nodes of forward/backward distance up to `cutoff` starting from the input `node` in a directed graph `dEdges`
nodesForward,nodesBackward :: (UV.Unbox a, Ord a) => UV.Vector (a,a) -> a -> Dist -> UV.Vector (a,Dist)
nodesForward dEdges node cutoff  = accumReachable to accum cutoff dForwardEdges (UV.fromList [node],0)
  where
    accum = UV.fromList [(node,0)]
    dForwardEdges = neighbor dEdges from
nodesBackward dEdges node cutoff = accumReachable from accum cutoff dBackwardEdges (UV.fromList [node],0)
  where
    accum = UV.fromList [(node,0)]
    dBackwardEdges = neighbor dEdges to

{-
accumPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> UV.Vector a -> V.Vector (UV.Vector a)
accumPaths fn path = UV.foldl' f V.empty (UV.map snd (fn from))
  where
    from = UV.last path
    f accum to = V.snoc accum (UV.snoc path to)

allPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> a -> Dist -> V.Vector (UV.Vector a)
allPaths fn node cutoff = f 0 (V.singleton (UV.singleton node))
  where
    f dist paths | dist==cutoff = paths
    f dist paths = f (dist+1) (V.concatMap (accumPaths fn) paths)
-}

--accumPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> UV.Vector a -> B.Bundle UV.Vector a
accumPaths fn path = UV.foldl' f B.empty (UV.map snd (fn from))
  where
    from = UV.last path
    f accum to = B.snoc accum (UV.snoc path to)

-- all paths of length `cutoff`, starting from the input `node`
--allPathsOf :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> a -> Dist -> V.Vector (UV.Vector a)
allPathsOf fn node cutoff = f 0 (B.singleton (UV.singleton node))
  where
    f dist paths | dist==cutoff = paths
    f dist paths = f (dist+1) (B.concatMap (accumPaths fn) paths)

-- all paths of length UPTO `cutoff`, starting from the input `node`
--allPathsUpto :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> a -> Dist -> B.Bundle UV.Vector a
allPathsUpto fn node cutoff = f B.empty 0 (B.singleton (UV.singleton node))
  where
    f accum dist paths | dist==cutoff = accum B.++ paths
    f accum dist paths = f (accum B.++ paths) (dist+1) nexts
      where
        nexts = B.concatMap (accumPaths fn) paths

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

    dForwardEdges  = neighbor directed from
    dBackwardEdges = neighbor directed to
    uForwardEdges  = neighbor undirected from
    uBackwardEdges = neighbor undirected to

  eassertEqual (dForwardEdges  1) (UV.fromList [(1,2),(1,3),(1,5),(1,6)])
  eassertEqual (dBackwardEdges 1) (UV.fromList [(4,1),(9,1),(8,1),(10,1)])
  eassertEqual (uBackwardEdges 4) (UV.fromList [(3,4),(2,4),(1,4)])
  eassertEqual (uForwardEdges  4) (UV.fromList [(4,2),(4,1),(4,3)])

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

u :: [[Int64]] -> [UV.Vector Int64]
u = map UV.fromList

testAllPaths :: TestTree
testAllPaths = testCaseSteps "Get all paths within distance cutoff between a pair of nodes" $ \step -> do
  let
    e from to = (NodeID from, NodeID to)
    directed = vectorizeEdges ([ e 1 2, e 1 3, e 2 4, e 3 4, e 4 1
                               , e 1 5, e 5 6, e 5 7, e 1 6
                               , e 9 1, e 8 1, e 10 9,e 10 8,e 10 1
                               , e 8 11,e 11 3
                               ] :: [Edge])
    dForwardEdges  = neighbor directed from
    
  eassertEqual (u [[8,1],[8,11]]) (B.toList (accumPaths dForwardEdges (UV.fromList [8])))
  eassertEqual (u [[8,8,1],[8,8,11]]) (B.toList (accumPaths dForwardEdges (UV.fromList [8,8]))) --Nonsense input. Just for testing
  eassertEqual (u [[10,9],[10,8],[10,1]]) (B.toList (accumPaths dForwardEdges (UV.fromList [10])))
  let
    tmp = allPathsOf dForwardEdges 10 3
    tmp2 = allPathsUpto dForwardEdges 10 3
  eassertEqual (B.toList tmp)  (u [[10,9,1,2],[10,9,1,3],[10,9,1,5],[10,9,1,6],[10,8,1,2],[10,8,1,3],[10,8,1,5],[10,8,1,6],[10,8,11,3],[10,1,2,4],[10,1,3,4],[10,1,5,6],[10,1,5,7]])
  eassertEqual (B.toList tmp2) (u [[10],[10,9],[10,8],[10,1],[10,9,1],[10,8,1],[10,8,11],[10,1,2],[10,1,3],[10,1,5],[10,1,6],[10,9,1,2],[10,9,1,3],[10,9,1,5],[10,9,1,6],[10,8,1,2],[10,8,1,3],[10,8,1,5],[10,8,1,6],[10,8,11,3],[10,1,2,4],[10,1,3,4],[10,1,5,6],[10,1,5,7]])




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
