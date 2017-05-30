{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiEntityTagger where

import           Data.Maybe                            (fromJust, isNothing)
import           Data.List                             (inits, transpose)
import           Data.Text                             (Text)
import           Control.Monad.Primitive               (PrimMonad, PrimState)
import           Control.Monad.ST                      (ST, runST)
import           Control.Arrow                         (second)
import           Data.Vector.Generic.Mutable           (MVector)
import           Data.Vector                           (Vector,backpermute,findIndices
                                                       ,slice,fromList,toList,unsafeThaw,modify)
import           Data.Ord                              (Ord)

import           Misc                                  (IRange(..))
import           Assert                                (massertEqual,eassertEqual)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           WikiEntity                            (parseEntityLine,loadEntityReprs,nameWords)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Search as VS
import qualified WikiEntity                    as Wiki
{-
import qualified Data.Vector.Unboxed.Mutable   as MV
import qualified Data.Vector.Unboxed           as V
-}


 
ithElementOrdering :: (Ord e) => Int -> [e] -> [e] -> Ordering
ithElementOrdering i lhs rhs | length lhs <= i = LT
                             | length rhs <= i = GT
                             | otherwise = compare (lhs!!i) (rhs!!i)

binarySearchLR :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> e -> m (Int,Int)
binarySearchLR vec elm = do
  idxL <- VS.binarySearchL vec elm
  idxR <- VS.binarySearchR vec elm
  return (idxL, idxR)  

binarySearchLRBy :: (PrimMonad m, MVector v e) => VS.Comparison e -> v (PrimState m) e -> e -> m (Int,Int)
binarySearchLRBy comp vec elm = do
  idxL <- VS.binarySearchLBy comp vec elm
  idxR <- VS.binarySearchRBy comp vec elm
  return (idxL, idxR)  

binarySearchLRByBounds :: (PrimMonad m, MVector v e) => VS.Comparison e -> v (PrimState m) e -> e -> Int -> Int -> m (Int,Int)
binarySearchLRByBounds comp vec elm l u = do
  idxL <- VS.binarySearchLByBounds comp vec elm l u
  idxR <- VS.binarySearchRByBounds comp vec elm l u
  return (idxL, idxR)  

greedyMatchImpl :: (Ord e) => Vector [e] -> [e] -> (Int, IRange) -> (Int, IRange)
greedyMatchImpl entities words (i, IRange beg end) = runST $ do
  mvec <- unsafeThaw entities
  (idxL, idxR) <- binarySearchLRByBounds (ithElementOrdering i) mvec words beg end
  --return (i, IRange beg end)
  if idxL==idxR
    then return (i, IRange beg end)
    else return (greedyMatchImpl entities words (i+1, IRange idxL idxR))

greedyMatch :: (Ord e) => Vector [e] -> [e] -> (Int, IRange)
greedyMatch entities words = greedyMatchImpl entities words (0, IRange 0 (length entities))

getMatchedIndexes :: Vector [e] -> (Int, IRange) -> (Int, Vector Int)
getMatchedIndexes vec (len, IRange beg end) = (len, matchedItems)
  where 
    tmp          = findIndices (\x-> length x == len) vec
    matchedItems = V.filter (\x-> x>=beg && x<end) tmp

greedyMatchedItems :: (Ord e) => Vector [e] -> [e] -> (Int, Vector Int)
greedyMatchedItems entities words = getMatchedIndexes entities (greedyMatch entities words)

greedyAnnotationImpl :: (Ord e) => Vector [e] -> [e] -> Int -> [(IRange, Vector Int)] -> [(IRange, Vector Int)]
greedyAnnotationImpl entities []   offset results = results
greedyAnnotationImpl entities text offset results = 
  let
    (len, matched) = greedyMatchedItems entities text
    r = (IRange offset (offset+len), matched)
  in
    if len==0 || null matched
      then greedyAnnotationImpl entities (tail text) (offset+1) results
      else greedyAnnotationImpl entities (drop len text) (offset+len) (r:results)
  
greedyAnnotation :: (Ord e) => Vector [e] -> [e] -> [(IRange, Vector Int)]
greedyAnnotation entities text = greedyAnnotationImpl entities text 0 []


itemTuple :: (Wiki.UID, Wiki.Name) -> (Wiki.UID, [Text])
itemTuple (uid, name) = (uid, nameWords name)

data NameUIDTable = NameUIDTable { _uids :: Vector Wiki.UID
                                 , _names :: Vector [Text]}
                  deriving (Show)

buildEntityTable :: [(Wiki.UID, Wiki.Name)] -> NameUIDTable
buildEntityTable entities = NameUIDTable uids names
  where
    nameOrdering (lhsUID, lhsName) (rhsUID, rhsName) = compare lhsName rhsName
    entitiesByName = modify (sortBy nameOrdering) (fromList (map itemTuple entities))
    uids  = V.map fst entitiesByName
    names = V.map snd entitiesByName

wikiAnnotator:: NameUIDTable -> [Text] -> [(IRange, Vector Wiki.UID)]
wikiAnnotator entities words = matchedItems
  where
    matchedIdxs  = greedyAnnotation (_names entities) words
    matchedItems = map (second (V.map (V.unsafeIndex (_uids entities)))) matchedIdxs