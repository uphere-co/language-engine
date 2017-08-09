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

accumReachable :: (UV.Unbox a, Ord a) => UV.Vector (a,Int32) -> Int32 -> (a -> UV.Vector (a,a)) -> (UV.Vector a, Int32) ->  UV.Vector (a,Int32)
accumReachable accum cutoff dfn (frontiers,dist) | cutoff==dist = accum
accumReachable accum cutoff dfn (frontiers,dist) = accumReachable (UV.concat [ns, accum]) cutoff dfn (nexts, dist+1)
  where
    f dfn node = UV.map snd (dfn node)
    reachable = UV.concatMap (f dfn) frontiers
    news = UV.filter (not . isIn (UV.map fst accum)) reachable
    nexts = unique news
    ns = UV.map (\x -> (x, dist+1)) nexts

nodesForward :: (UV.Unbox a, Ord a) => UV.Vector (a,a) -> a -> Int32 -> UV.Vector (a,Int32)
nodesForward dEdges node cutoff = accumReachable accum cutoff dForwardNodes (UV.fromList [node],0)
  where
    accum = UV.fromList [(node,0)]
    dForwardNodes = neighbor dEdges orderingByFrom

testNeighborNodes :: TestTree
testNeighborNodes = testCaseSteps "Get neighbor nodes in directed/undirected graph" $ \step -> do
  let
    -- (from,to)
    directed = UV.fromList ([(1,2),(1,3),(2,4),(3,4),(4,1)
                            ,(1,5),(5,6),(5,7),(1,6)
                            ,(9,1),(8,1),(10,9),(10,8),(10,1)
                            ,(8,11),(11,3)
                            ] :: [(Int32, Int32)])
    undirected = UV.concatMap (\(x,y) -> UV.fromList [(x,y),(y,x)]) directed
    empty = UV.fromList ([]::[(Int32,Int32)])

  let
    dForwardNodes  = neighbor directed orderingByFrom
    dBackwardNodes = neighbor directed orderingByTo
    uForwardNeighbor  = neighbor undirected orderingByFrom
    uBackwardNeighbor = neighbor undirected orderingByTo
  eassertEqual (dForwardNodes  1) (UV.fromList [(1,2),(1,3),(1,5),(1,6)])
  eassertEqual (dBackwardNodes 1) (UV.fromList [(4,1),(9,1),(8,1),(10,1)])
  eassertEqual (uBackwardNeighbor 4) (UV.fromList [(3,4),(2,4),(1,4)])
  eassertEqual (uForwardNeighbor  4) (UV.fromList [(4,2),(4,1),(4,3)])

  eassertEqual (nodesForward directed 10 2) (UV.fromList [(2,2),(3,2),(5,2),(6,2),(11,2),(1,1),(8,1),(9,1),(10,0)])
  eassertEqual (nodesForward directed 10 3) (UV.fromList [(4,3),(7,3),(2,2),(3,2),(5,2),(6,2),(11,2),(1,1),(8,1),(9,1),(10,0)])
  eassertEqual (nodesForward directed 3  3) (UV.fromList [(2,3),(5,3),(6,3),(1,2),(4,1),(3,0)])


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

fff :: TestTree
fff = testCaseSteps "ff" $ \step -> do
  print "a"

allTest :: TestTree
allTest =
  testGroup
    "All graph operation unit tests"
    [ testNeighborNodes
    , testUtilsForShortedPath
    , fff
    ]    
