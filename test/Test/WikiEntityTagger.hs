{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.WikiEntityTagger where

import           Data.Maybe                            (fromMaybe)
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,backpermute,findIndices
                                                       ,slice,fromList,toList,unsafeThaw,modify)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.WikiEntityClass                (SuperclassUID(..),SubclassUID(..),fromRows,buildRelations,allRelationPairs,getAncestors,isSubclass)
import           WikiEL.Misc                           (IRange(..))
import           Assert                                (assert,massertEqual,eassertEqual)
import           WikiEL.WikiEntityTagger
import           WikiEL.BinarySearch                   (binarySearchLR,binarySearchLRBy,binarySearchLRByBounds)


import           WikiEL.Type.Wikidata
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser
import           WikiEL.ETL.LoadData

import           Test.Data.Filename

uid = itemID

testVectorSlicing :: TestTree
testVectorSlicing = testCaseSteps "API usages for vector slicing" $ \step -> do
  let 
    vec = fromList ([[1],[2],[3,4],[5,6],[7]] :: [[Int]])
    sub = slice 1 3 vec
  eassertEqual (toList (slice 1 1 vec)) [[2]]
  eassertEqual (toList (slice 2 2 vec)) [[3,4],[5,6]]
  eassertEqual (toList sub) [[2],[3,4],[5,6]]
  eassertEqual (filter (\x -> length x == 2) (toList sub)) [[3,4],[5,6]]


[a,b,c,d,e,f,g,  x,y,z] = [10,11,12,13,14,15,16,  100,101,102] :: [WordHash]

testBinarySearch :: TestTree
testBinarySearch = testCaseSteps "API usages for binary searches" $ \step -> do
  let
    wordss = fromList (map UV.fromList ([[b], [b, c], [b, b], [b,c,b],  [a,b], [a], [b], [b], [a, c], [c],[c], [c, b], [e,a], [e], [g]] :: [[WordHash]]))
    wordssSorted = map UV.fromList ([[a],[a,b],[a,c],[b],[b],[b],[b,b],[b,c],[b,c,b],[c],[c],[c,b], [e], [e,a], [g]] :: [[WordHash]])
  
  tt <- V.thaw wordss
  sort tt  
  massertEqual (V.freeze tt) (fromList wordssSorted)
  
  step "binarySearchLR"
  massertEqual (binarySearchLR tt (UV.fromList [b])) (3,6)
  massertEqual (binarySearchLR tt (UV.fromList [c])) (9,11)  
  massertEqual (binarySearchLR tt (UV.fromList [d])) (12,12)
  massertEqual (binarySearchLRBy (ithElementOrdering 0) tt (UV.fromList [d])) (12,12)
  
  step "binarySearchLRBy"
  let
     bc = UV.fromList [b, c]
     eb = UV.fromList [e, b]
  (bidxBL0, bidxBR0) <- binarySearchLRBy (ithElementOrdering 0) tt bc
  eassertEqual (bidxBL0, bidxBR0) (3,9)
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt bc bidxBL0 bidxBR0) (7,9)
  (_, _) <- binarySearchLRByBounds (ithElementOrdering 1) tt bc 3 6  

  (tl0, tr0) <- binarySearchLRBy (ithElementOrdering 0) tt eb
  eassertEqual (tl0, tr0) (12,14)
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt eb tl0 tr0) (14,14)

unitTestsVector :: TestTree
unitTestsVector =
  testGroup
    "Usages for Data.Vector and Data.Vector.Algorithms"
    [testVectorSlicing, testBinarySearch]


testNameOrdering :: TestTree
testNameOrdering = testCaseSteps "Ordering of entity names(list of words)" $ \step -> do
  eassertEqual LT (ithElementOrdering 0 (UV.fromList [a, b]) (UV.fromList [b, a]))
  eassertEqual GT (ithElementOrdering 1 (UV.fromList [a, b]) (UV.fromList [b, a]))
  eassertEqual EQ (ithElementOrdering 1 (UV.fromList [a, a]) (UV.fromList [a, a, a]))

testGreedyMatching :: TestTree
testGreedyMatching = testCaseSteps "Greedy matching of two lists of words" $ \step -> do
  let 
    entities = fromList (map UV.fromList ([[a], [b], [b,c], [b,d,e],[b,d,f],[c],[c,d,e,f],[c,d,e,f]] :: [[WordHash]]))
    words    = [x, a,b, z] :: [WordHash]
  step "Null cases"
  eassertEqual (greedyMatch entities (UV.fromList ([]::[WordHash])))    (0, IRange 0 8)
  step "Single word cases"
  eassertEqual (greedyMatch entities (UV.fromList [x])) (0, IRange 0 8)
  eassertEqual (greedyMatch entities (UV.fromList [b])) (1, IRange 1 5)
  step "Multi words cases"
  eassertEqual (greedyMatch entities (UV.fromList [b,c,x,y])) (2, IRange 2 3)
  eassertEqual (greedyMatch entities (UV.fromList [b,d,x,y])) (2, IRange 3 5)
  eassertEqual (greedyMatch entities (UV.fromList [b,d,e,f])) (3, IRange 3 4)
  eassertEqual (greedyMatch entities (UV.fromList [c,d,e,f])) (4, IRange 6 8)

  step "Single run for entity tagging"
  eassertEqual (greedyMatchedItems entities (UV.fromList [b,c,x,y,z]))    (2, fromList [2])
  eassertEqual (greedyMatchedItems entities (UV.fromList [x, b,c,x,y,z])) (0, fromList [])
  
  step "Recursive tagging"
  let
    text = UV.fromList [x, b,c,x,y,z, a, b,d,f, x,c,d,c,d,e,f,b]
    expected = [(IRange 17 18, fromList [1])
               ,(IRange 13 17, fromList [6,7])
               ,(IRange 7 10,  fromList [4])
               ,(IRange 6 7,   fromList [0])
               ,(IRange 1 3,   fromList [2])]
  eassertEqual (greedyAnnotation entities text) expected  

unitTestsGreedyMatching :: TestTree
unitTestsGreedyMatching =
  testGroup
    "Text based, greedy matching algorithm for list of words"
    [testNameOrdering, testGreedyMatching]


testWikiEntityTypes :: TestTree
testWikiEntityTypes = testCaseSteps "Test on hierarchy of Wiki entity types" $ \step -> do
  let
    lines = ["Q12\t12\tQ121\t121"
            , "Q12\t12\tQ122\t122"
            , "Q11\t11\tQ111\t111"
            , "Q11\t11\tQ112\t112"
            , "Q1\t1\tQ11\t11"
            , "Q1\t1\tQ12\t12"
            ]
    relTuples = fromRows (map subclassRelation lines)
    relations = buildRelations relTuples
    pairs = allRelationPairs relTuples

    super id = SuperclassUID (uid id)
    sub   id = SubclassUID   (uid id)
  eassertEqual (getAncestors relations (uid "Q1")) [uid "Q1",uid "Q12",uid "Q122",uid "Q121",uid "Q11",uid "Q112",uid "Q111"]
  assert (isSubclass pairs (super "Q122") (sub "Q1"))
  assert (not (isSubclass pairs (super "Q122") (sub "Q11")))
  mapM_ T.IO.putStrLn lines




testWikiEntityTagging :: TestTree
testWikiEntityTagging = testCaseSteps "Wiki entity tagger with greedy-matching strategy" $ \step -> do
  entities <- do
     reprs <- loadEntityReprs reprFileTiny
     return (buildEntityTable reprs)
  let
    text = "Google and Facebook Inc. are famous AI companies . NLP stands for natural language processing ."
    words = T.words text    
    matchedItems  = wikiAnnotator entities words
    expected = [(IRange 12 15, fromList [uid "Q30642"])
               ,(IRange 9 10,  fromList [uid "Q30642"])
               ,(IRange 6 7,   fromList [uid "Q11660", uid "Q42970"])
               ,(IRange 2 4,   fromList [uid "Q380"])
               ,(IRange 0 1,   fromList [uid "Q95", uid "Q9366"])
               ]
  eassertEqual matchedItems expected
  --print ""
  --mapM_ print matchedItems


allTest :: TestTree
allTest =
  testGroup
    "All Unit tests"
    [unitTestsVector, unitTestsGreedyMatching, testWikiEntityTagging, testWikiEntityTypes]    
