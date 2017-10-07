{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module WikiEL.WikiEntityTagger 
  ( module WikiEL.WikiEntityTagger
  , WordHash
  ) where

import           Data.Maybe                            (fromJust, isNothing)
import           Data.List                             (inits, transpose)
import           Data.Text                             (Text)
import           Control.Monad.ST                      (ST, runST)
import           Control.Arrow                         (second)
import           Data.Vector                           (Vector,backpermute,findIndices
                                                       ,slice,fromList,toList,unsafeThaw,modify)
import           Data.Vector.Unboxed                   ((!))
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Search as VS
import qualified Data.Vector.Unboxed           as UV


import           Graph.Internal.Hash                   (WordHash,wordHash)
--import           WikiEL.Util.Hash                      (WordHash,wordHash)
import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Type.FileFormat                (EntityReprFile,EntityReprRow(..))
import           WikiEL.ETL.LoadData                   (loadEntityReprs)
import           WikiEL.Misc                           (IRange(..))
import           Assert                                (massertEqual,eassertEqual)
import           WikiEL.BinarySearch                   (binarySearchLR,binarySearchLRBy,binarySearchLRByBounds)
import qualified WikiEL.Type.Wikidata         as Wiki

{-|
  WikiEL.WikiEntityTagger is a text-match based Wikipedia/Wikidata entity tagger. 
  With greedy algorithm, it tags a phrase if it matches with a name or an alias of an entity.
-}


type WordsHash = UV.Vector WordHash

ithElementOrdering :: Int -> WordsHash -> WordsHash -> Ordering
ithElementOrdering i lhs rhs | UV.length lhs <= i = LT
                             | UV.length rhs <= i = GT
                             | otherwise = compare (lhs!i) (rhs!i)


greedyMatchImpl :: Vector WordsHash -> WordsHash -> (Int, IRange) -> (Int, IRange)
greedyMatchImpl entities words (i, IRange beg end) = runST $ do
  mvec <- unsafeThaw entities
  (idxL, idxR) <- binarySearchLRByBounds (ithElementOrdering i) mvec words beg end
  --return (i, IRange beg end)
  if idxL==idxR
    then return (i, IRange beg end)
    else return (greedyMatchImpl entities words (i+1, IRange idxL idxR))

greedyMatch :: Vector WordsHash -> WordsHash -> (Int, IRange)
greedyMatch entities words = greedyMatchImpl entities words (0, IRange 0 (length entities))

getMatchedIndexes :: Vector WordsHash -> (Int, IRange) -> (Int, Vector Int)
getMatchedIndexes vec (len, IRange beg end) = (len, matchedIdxs)
  where
    ivec = V.indexed vec
    tmp  = V.slice beg (end-beg) ivec
    f (i,x) | UV.length x == len = Just i
    f _ = Nothing
    matchedIdxs = V.mapMaybe f tmp    

greedyMatchedItems :: Vector WordsHash-> WordsHash-> (Int, Vector Int)
greedyMatchedItems entities words = getMatchedIndexes entities (greedyMatch entities words)

greedyAnnotationImpl :: Vector WordsHash -> WordsHash -> Int -> [(IRange, Vector Int)] -> [(IRange, Vector Int)]
greedyAnnotationImpl entities text offset results | text == (UV.empty :: WordsHash) = results
greedyAnnotationImpl entities text offset results = 
  let
    (len, matched) = greedyMatchedItems entities text
    r = (IRange offset (offset+len), matched)
  in
    if len==0 || null matched
      -- According to Hackage, UV.tail and UV.drop yield elements "without copying"
      then greedyAnnotationImpl entities (UV.tail text) (offset+1) results
      else greedyAnnotationImpl entities (UV.drop len text) (offset+len) (r:results)

-- Vector Int : indexes of matched elements. To be converted to ItemIDs using _uids. 
greedyAnnotation :: Vector WordsHash -> WordsHash -> [(IRange, Vector Int)]
greedyAnnotation entities text = greedyAnnotationImpl entities text 0 []

wordsHash :: [Text] -> WordsHash
wordsHash words = UV.fromList (map wordHash words)

nameWordsHash :: Wiki.ItemRepr -> WordsHash
nameWordsHash (Wiki.ItemRepr name) = wordsHash (T.words name)

itemTuple :: EntityReprRow -> (ItemID, WordsHash)
itemTuple (EntityReprRow uid name) = (uid, nameWordsHash name)

data NameUIDTable = NameUIDTable { _uids :: Vector ItemID
                                 , _names :: Vector WordsHash}
                  deriving (Show)

buildEntityTable :: [EntityReprRow] -> NameUIDTable
buildEntityTable entities = NameUIDTable uids names
  where
    nameOrdering (lhsUID, lhsName) (rhsUID, rhsName) = compare lhsName rhsName
    entitiesByName = modify (sortBy nameOrdering) (fromList (map itemTuple entities))
    uids  = V.map fst entitiesByName
    names = V.map snd entitiesByName

loadWETagger :: EntityReprFile -> IO NameUIDTable
loadWETagger file = do
     reprs <- loadEntityReprs file
     let 
       table = buildEntityTable reprs
     return table 


wikiAnnotator:: NameUIDTable -> [Text] -> [(IRange, Vector ItemID)]
wikiAnnotator entities words = matchedItems
  where
    tokens    = wordsHash words  
    matchedIdxs  = greedyAnnotation (_names entities) tokens
    matchedItems = map (second (V.map (V.unsafeIndex (_uids entities)))) matchedIdxs
