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

neighbor :: (UV.Unbox a, Ord a) => VS.Comparison (a,a) -> UV.Vector (a,a) -> a -> UV.Vector (a,a)
neighbor comp edges node= runST $ do
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

    dfl = UV.modify (sortBy orderingByFrom) directed
    dfr = UV.modify (sortBy orderingByTo)   directed
    ufl = UV.modify (sortBy orderingByFrom) undirected
    ufr = UV.modify (sortBy orderingByTo)   undirected

  print dfl
  print dfr
  print ufl
  print ufr
  eassertEqual (neighbor orderingByFrom dfl 1) (UV.fromList [(1,2),(1,3),(1,5),(1,6)])
  eassertEqual (neighbor orderingByTo   dfr 1) (UV.fromList [(4,1),(9,1),(8,1),(10,1)])


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
