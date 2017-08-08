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

  let
    dNeighborForward  = neighbor directed orderingByFrom
    dNeighborBackward = neighbor directed orderingByTo
    uNeighborForward  = neighbor undirected orderingByFrom
    uNeighborBackward = neighbor undirected orderingByTo
  eassertEqual (dNeighborForward  1) (UV.fromList [(1,2),(1,3),(1,5),(1,6)])
  eassertEqual (dNeighborBackward 1) (UV.fromList [(4,1),(9,1),(8,1),(10,1)])
  eassertEqual (uNeighborBackward 4) (UV.fromList [(3,4),(2,4),(1,4)])
  eassertEqual (uNeighborForward  4) (UV.fromList [(4,2),(4,1),(4,3)])
  print $ UV.concatMap dNeighborForward (UV.fromList [1,10])

isIn :: (UV.Unbox a, Ord a) => UV.Vector a -> a -> Bool
isIn vals = f sorted
  where
    sorted = UV.modify sort vals
    f vs x = runST $ do
      mvec <- UV.unsafeThaw vs
      (beg,end) <- binarySearchLR mvec x
      --return (UV.slice beg (end-beg) vs)
      return ((end-beg)/=0)



testUtilsForShortedPath :: TestTree
testUtilsForShortedPath = testCaseSteps "Test helper functions for shorted path" $ \step -> do
  let
    nodes = UV.fromList ([1,3,6,2,9,7] :: [Int32])
  assert (UV.all (isIn nodes) nodes)
  assert (not (isIn nodes 5))

ff :: TestTree
ff = testCaseSteps "ff" $ \step -> do
  print "a"

allTest :: TestTree
allTest =
  testGroup
    "All graph operation unit tests"
    [ testNeighborNodes
    , testUtilsForShortedPath
    , ff
    ]    
