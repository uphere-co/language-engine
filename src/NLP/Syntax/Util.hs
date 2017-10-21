{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Util where

import           Control.Lens                      ((^?),(%~),to)
import           Control.Monad                     (guard)
import           Data.Bifoldable                   (bifoldMap)
import           Data.IntMap                       (IntMap)
import           Data.Monoid                       (First(..))
--
import           CoreNLP.Simple.Convert            (lemmatize)
import           Data.Attribute
import           Data.Bitree
import           Data.BitreeZipper
import           Data.ListZipper
import           NLP.Type.PennTreebankII
import           NLP.Type.TagPos                   (TokIdx,BeginEnd)
import           NLP.Type.TagPos
--
import           NLP.Syntax.Type.XBar


class GetIntLemma tag where
  intLemma :: BitreeZipperICP tag -> Maybe (Int,Lemma)
  intLemma0 :: BitreeICP tag -> Maybe (Int,Lemma)

instance GetIntLemma (Lemma ': as) where
  intLemma :: BitreeZipperICP (Lemma ': as) -> Maybe (Int,Lemma)
  intLemma c = do
    i <- getLeafIndex (current c)
    l <- ahead . getAnnot <$> getLeaf (current c)
    return (i,l)

  intLemma0 :: BitreeICP (Lemma ': as) -> Maybe (Int,Lemma)
  intLemma0 c = do
    i <- getLeafIndex c
    l <- ahead . getAnnot <$> getLeaf c
    return (i,l)




phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)


getLeaf :: Bitree c (i,t) -> Maybe t
getLeaf (PL (_,x)) = Just x
getLeaf _          = Nothing


getLeafIndex :: Bitree c (i,t) -> Maybe i
getLeafIndex (PL (i,_)) = Just i
getLeafIndex _          = Nothing




isLemmaAs :: (GetIntLemma tag) => Lemma -> BitreeICP tag -> Bool
isLemmaAs lma c = maybe False ((==lma).snd) (intLemma0 c)


isPOSAs :: POSTag -> BitreeICP tag -> Bool
isPOSAs pos (PL (_,x)) = posTag x == pos
isPOSAs _   _          = False


isChunkAs :: ChunkTag -> BitreeICP as -> Bool
isChunkAs chk (PN (_,x) _) = chunkTag x == chk
isChunkAs _   _            = False


isVBN :: BitreeZipperICP a -> Bool
isVBN z = isPOSAs VBN (current z)


isVBG :: BitreeZipperICP a -> Bool
isVBG z = isPOSAs VBG (current z)


getIdxPOS :: BitreeICP a -> Maybe (Int,POSTag)
getIdxPOS w = (,) <$> getLeafIndex w <*> fmap posTag (getLeaf w)


mkBitreeICP :: IntMap Lemma -> PennTree -> BitreeICP '[Lemma]
mkBitreeICP lemmamap = lemmatize lemmamap . mkAnnotatable . mkPennTreeIdx


beginEndToRange :: BeginEnd TokIdx -> Range
beginEndToRange (TokIdx b,TokIdx e) = (b,e-1)


findZipperForRangeICP :: Range -> BitreeICP a -> Maybe (Zipper a)
findZipperForRangeICP rng tr = getFirst (bifoldMap check check (mkBitreeZipper [] tr))
  where check z = First $ do
                    rng' <-  z ^? to current . to getRange
                    guard (rng' == rng)
                    return z




mergeLeftELZ :: Either (ListZipper a) [a] -> Either (ListZipper a) [a] -> Either (ListZipper a) [a]
mergeLeftELZ (Right xs) (Right ys) = Right (xs++ys)
mergeLeftELZ (Left z1)  (Right ys) = Left ((lz_nexts %~ (++ ys)) z1)
mergeLeftELZ (Right xs) (Left z2)  = Left ((lz_prevs %~ (++ (reverse xs))) z2)
mergeLeftELZ (Left z1)  (Left z2)  = Left (mergeLeftLZ z1 z2)

mergeRightELZ :: Either (ListZipper a) [a] -> Either (ListZipper a) [a] -> Either (ListZipper a) [a]
mergeRightELZ (Right xs) (Right ys) = Right (xs++ys)
mergeRightELZ (Left z1)  (Right ys) = Right (lzToList z1 ++ ys)
mergeRightELZ (Right xs) (Left z2)  = Left ((lz_prevs %~ (++ (reverse xs))) z2)
mergeRightELZ (Left z1)  (Left z2)  = Left (mergeRightLZ z1 z2)
