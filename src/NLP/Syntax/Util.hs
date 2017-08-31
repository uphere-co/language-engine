{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Util where

import           Data.Attribute
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Type.XBar



type AuxNegWords w = (Maybe (w,(Int,Lemma)),Maybe (w,(Int,Lemma)),[(w,(Int,Lemma))])


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


