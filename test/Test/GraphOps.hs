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

import           WikiEL.BinarySearch                   (binarySearchLRBy)



orderingByFrom,orderingByTo :: Ord a => (a,a) -> (a,a) -> Ordering
orderingByFrom (lf,lt) (rf,rt) = compare lf rf
orderingByTo   (lf,lt) (rf,rt) = compare lt rt

neighbor :: (UV.Unbox a, Ord a) => UV.Vector (a,a) -> VS.Comparison (a,a) -> a -> UV.Vector (a,a)
neighbor nodes comp node = f sorted comp node
  where
    sorted = UV.modify (sortBy comp) nodes
    f edges comp node= runST $ do
      mvec <- UV.unsafeThaw edges
      (beg, end) <- binarySearchLRBy comp mvec (node,node)
      return (UV.slice beg (end-beg) edges)

testNeighborNodes :: TestTree
testNeighborNodes = testCaseSteps "Get neighbor nodes in directed/undirected graph" $ \step -> do
  let
    -- (from,to)
    directed = UV.fromList ([(1,2),(1,3),(2,4),(3,4),(4,1)
                            ,(1,5),(5,6),(5,7),(1,6)
                            ,(9,1),(8,1),(10,9),(10,8),(10,1)
                            ] :: [(Int32, Int32)])
    undirected = UV.concatMap (\(x,y) -> UV.fromList [(x,y),(y,x)]) directed

  let
    dNeighborForward  = neighbor directed orderingByFrom
    dNeighborBackward = neighbor directed orderingByTo
    uNeighborForward  = neighbor undirected orderingByFrom
    uNeighborBackward = neighbor undirected orderingByTo
  eassertEqual (dNeighborForward  1) (UV.fromList [(1,2),(1,3),(1,5),(1,6)])
  eassertEqual (dNeighborBackward 1) (UV.fromList [(4,1),(9,1),(8,1),(10,1)])
  eassertEqual (uNeighborBackward 4) (UV.fromList [(3,4),(1,4),(2,4)])
  eassertEqual (uNeighborForward  4) (UV.fromList [(4,1),(4,2),(4,3)])


ff :: TestTree
ff = testCaseSteps "ff" $ \step -> do
  assert (True)


allTest :: TestTree
allTest =
  testGroup
    "All graph operation unit tests"
    [ testNeighborNodes
    , ff
    ]    
