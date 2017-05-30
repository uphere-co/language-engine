{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
  
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,backpermute,findIndices
                                                       ,slice,fromList,toList,unsafeThaw,modify)

import           Misc                                  (IRange(..))
import           Assert                                (massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import           WikiEntityTagger
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified WikiEntity                    as Wiki


testVectorSlicing = testCaseSteps "API usages for vector slicing" $ \step -> do
  let 
    vec = fromList ([[1],[2],[3,4],[5,6],[7]] :: [[Int]])
    sub = slice 1 3 vec
  eassertEqual (toList (slice 1 1 vec)) [[2]]
  eassertEqual (toList (slice 2 2 vec)) [[3,4],[5,6]]
  eassertEqual (toList sub) [[2],[3,4],[5,6]]
  eassertEqual (filter (\x -> length x == 2) (toList sub)) [[3,4],[5,6]]

testBinarySearch = testCaseSteps "API usages for binary searches" $ \step -> do
  let
    wordss = fromList ([["B"], ["B", "C"], ["B", "B"], ["B","C","B"],  ["A","B"], ["A"], ["B"], ["B"], ["A", "C"], ["C"],["C"], ["C", "B"], ["E","A"], ["E"], ["G"]] :: [[Text]])
    wordssSorted = [["A"],["A","B"],["A","C"],["B"],["B"],["B"],["B","B"],["B","C"],["B","C","B"],["C"],["C"],["C","B"], ["E"], ["E","A"], ["G"]] :: [[Text]]
  
  tt <- V.thaw wordss
  sort tt  
  massertEqual (V.freeze tt) (fromList wordssSorted)
  
  step "binarySearchLR"
  massertEqual (binarySearchLR tt ["B"]) (3,6)
  massertEqual (binarySearchLR tt ["C"]) (9,11)  
  massertEqual (binarySearchLR tt ["D"]) (12,12)
  massertEqual (binarySearchLRBy (ithElementOrdering 0) tt ["D"]) (12,12)
  
  step "binarySearchLRBy"
  (bidxBL0, bidxBR0) <- binarySearchLRBy (ithElementOrdering 0) tt ["B", "C"]
  eassertEqual (bidxBL0, bidxBR0) (3,9)
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] bidxBL0 bidxBR0) (7,9)
  (_, _) <- binarySearchLRByBounds (ithElementOrdering 1) tt ["B", "C"] 3 6  

  (tl0, tr0) <- binarySearchLRBy (ithElementOrdering 0) tt ["E", "B"]
  eassertEqual (tl0, tr0) (12,14)
  massertEqual (binarySearchLRByBounds (ithElementOrdering 1) tt ["E", "B"] tl0 tr0) (14,14)

unitTestsVector =
  testGroup
    "Usages for Data.Vector and Data.Vector.Algorithms"
    [testVectorSlicing, testBinarySearch]


testNameOrdering = testCaseSteps "Ordering of entity names(list of words)" $ \step -> do
  eassertEqual LT (ithElementOrdering 0 ["A", "B"] ["B", "A"])
  eassertEqual GT (ithElementOrdering 1 ["A", "B"] ["B", "A"])
  eassertEqual EQ (ithElementOrdering 1 ["A", "A"] ["A", "A", "A"])

testGreedyMatching = testCaseSteps "Greedy matching of two lists of words" $ \step -> do
  let 
    entities = fromList ([["A"], ["B"], ["B","C"], ["B","D","E"],["B","D","F"],["C"],["C","D","E","F"],["C","D","E","F"]] :: [[Text]])
    words    = ["X", "A","B", "Z"] :: [Text]
  step "Null cases"
  eassertEqual (greedyMatch entities [])    (0, IRange 0 8)
  step "Single word cases"
  eassertEqual (greedyMatch entities ["X"]) (0, IRange 0 8)
  eassertEqual (greedyMatch entities ["B"]) (1, IRange 1 5)
  step "Multi words cases"
  eassertEqual (greedyMatch entities ["B","C","X","Y"]) (2, IRange 2 3)
  eassertEqual (greedyMatch entities ["B","D","X","Y"]) (2, IRange 3 5)
  eassertEqual (greedyMatch entities ["B","D","E","F"]) (3, IRange 3 4)
  eassertEqual (greedyMatch entities ["C","D","E","F"]) (4, IRange 6 8)

  step "Single run for entity tagging"
  eassertEqual (greedyMatchedItems entities ["B","C","X","Y","Z"]) (2, fromList [2])
  eassertEqual (greedyMatchedItems entities ["X", "B","C","X","Y","Z"]) (0, fromList [])
  
  step "Recursive tagging"
  let
    text = ["X", "B","C","X","Y","Z", "A", "B","D","F", "X","C","D","C","D","E","F","B"]
    expected = [(IRange 17 18, fromList [1])
               ,(IRange 13 17, fromList [6,7])
               ,(IRange 7 10,  fromList [4])
               ,(IRange 6 7,   fromList [0])
               ,(IRange 1 3,   fromList [2])]
  eassertEqual (greedyAnnotation entities text) expected  

unitTestsGreedyMatching =
  testGroup
    "Text based, greedy matching algorithm for list of words"
    [testNameOrdering, testGreedyMatching]


testWikiEntityTagging = testCaseSteps "Wiki entity tagger with greedy-matching strategy" $ \step -> do
  entities <- do
     reprs <- loadEntityReprs "../rnn++/tests/data/wikidata.test.entities"
     return (buildEntityTable reprs)
  let
    text = "Google and Facebook Inc. are famous AI companies . NLP stands for natural language processing ."
    words = T.words text    
    matchedItems  = wikiAnnotator entities words
    wuid = Wiki.UID
    expected = [(IRange 12 15, fromList [wuid "Q30642"])
               ,(IRange 9 10,  fromList [wuid "Q30642"])
               ,(IRange 6 7,   fromList [wuid "Q42970", wuid"Q11660"])
               ,(IRange 2 4,   fromList [wuid "Q380"])
               ,(IRange 0 1,   fromList [wuid "Q95", wuid "Q9366"])
               ]
  eassertEqual matchedItems expected
  --print ""
  --mapM_ print matchedItems

unitTests =
  testGroup
    "All Unit tests"
    [unitTestsVector, unitTestsGreedyMatching, testWikiEntityTagging]    

main = defaultMain unitTests
