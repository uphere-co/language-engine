{-# LANGUAGE ExplicitNamespaces #-}
module WikiEL.Match where

import           Control.Monad.Primitive     ( PrimMonad, PrimState )
import           Control.Monad.ST            ( runST )
import           Data.Vector                 ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Search as VS
import           Data.Vector.Generic.Mutable ( MVector )
import qualified Data.Vector.Unboxed as UV
------ wiki-ner
import           WikiEL.Type                 ( IRange(..)
                                             , type WordsHash
                                             )
import           WikiEL.Util                 ( ithElementOrdering )




-- old WikiEL.BinarySearch


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


-- old WikiEL.WikiEntityTagger


greedyMatchImpl :: Vector WordsHash -> WordsHash -> (Int, IRange) -> (Int, IRange)
greedyMatchImpl entities ws (i, IRange beg end) = runST $ do
  mvec <- V.unsafeThaw entities
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
