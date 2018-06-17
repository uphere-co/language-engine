{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module WikiEL.WikiEntityTagger 
  ( module WikiEL.WikiEntityTagger
  , WordHash
  ) where

import           Control.Monad.ST                      (runST)
import           Control.Arrow                         (second)
import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList,unsafeThaw,modify)
import           Data.Vector.Unboxed                   ((!))
import           Data.Vector.Algorithms.Intro          (sortBy)
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
--
import           Graph.Internal.Hash                   (WordHash,wordHash)
import           WikiEL.Type                           (IRange(..),NameUIDTable(..),WordsHash)
import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Type.FileFormat                (EntityReprFile,EntityReprRow(..))
import           WikiEL.ETL.LoadData                   (loadEntityReprs)
import           WikiEL.BinarySearch                   (binarySearchLRByBounds)
import qualified WikiEL.Type.Wikidata         as Wiki

{-|
  WikiEL.WikiEntityTagger is a text-match based Wikipedia/Wikidata entity tagger. 
  With greedy algorithm, it tags a phrase if it matches with a name or an alias of an entity.
-}


ithElementOrdering :: Int -> WordsHash -> WordsHash -> Ordering
ithElementOrdering i lhs rhs | UV.length lhs <= i = LT
                             | UV.length rhs <= i = GT
                             | otherwise = compare (lhs!i) (rhs!i)


greedyMatchImpl :: Vector WordsHash -> WordsHash -> (Int, IRange) -> (Int, IRange)
greedyMatchImpl entities ws (i, IRange beg end) = runST $ do
  mvec <- unsafeThaw entities
  (idxL, idxR) <- binarySearchLRByBounds (ithElementOrdering i) mvec ws beg end
  if idxL==idxR
    then return (i, IRange beg end)
    else return (greedyMatchImpl entities ws (i+1, IRange idxL idxR))


greedyMatch :: Vector WordsHash -> WordsHash -> (Int, IRange)
greedyMatch entities ws = greedyMatchImpl entities ws (0, IRange 0 (length entities))


getMatchedIndexes :: Vector WordsHash -> (Int, IRange) -> (Int, Vector Int)
getMatchedIndexes vec (len, IRange beg end) = (len, matchedIdxs)
  where
    ivec = V.indexed vec
    tmp  = V.slice beg (end-beg) ivec
    f (i,x) | UV.length x == len = Just i
    f _ = Nothing
    matchedIdxs = V.mapMaybe f tmp    


greedyMatchedItems :: Vector WordsHash-> WordsHash-> (Int, Vector Int)
greedyMatchedItems entities = getMatchedIndexes entities . greedyMatch entities


greedyAnnotationImpl :: Vector WordsHash -> WordsHash -> Int -> [(IRange, Vector Int)] -> [(IRange, Vector Int)]
greedyAnnotationImpl entities text offset results | text == (UV.empty :: WordsHash) = results
                                                  | otherwise  =
  let
    (len, matched) = greedyMatchedItems entities text
    r = (IRange offset (offset+len), matched)
  in
    if len==0 || null matched
      -- According to Hackage, UV.tail and UV.drop yield elements "without copying"
      then greedyAnnotationImpl entities (UV.tail text) (offset+1) results
      else greedyAnnotationImpl entities (UV.drop len text) (offset+len) (r:results)

--
-- | Vector Int : indexes of matched elements. To be converted to ItemIDs using _uids.
--
greedyAnnotation :: Vector WordsHash -> WordsHash -> [(IRange, Vector Int)]
greedyAnnotation entities text = greedyAnnotationImpl entities text 0 []


wordsHash :: [Text] -> WordsHash
wordsHash = UV.fromList . map wordHash


nameWordsHash :: Wiki.ItemRepr -> WordsHash
nameWordsHash (Wiki.ItemRepr name) = wordsHash (T.words name)


itemTuple :: EntityReprRow -> (ItemID, WordsHash)
itemTuple (EntityReprRow uid name) = (uid, nameWordsHash name)


buildEntityTable :: [EntityReprRow] -> NameUIDTable
buildEntityTable entities = NameUIDTable uids names
  where
    nameOrdering (_lhsUID, lhsName) (_rhsUID, rhsName) = compare lhsName rhsName
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
wikiAnnotator entities ws = matchedItems
  where
    tokens    = wordsHash ws  
    matchedIdxs  = greedyAnnotation (_names entities) tokens
    matchedItems = map (second (V.map (V.unsafeIndex (_uids entities)))) matchedIdxs
