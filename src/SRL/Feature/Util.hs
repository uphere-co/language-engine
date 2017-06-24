{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SRL.Feature.Util where

import           Control.Monad                  ((<=<))
import           Data.Bifoldable                (biList)
import           Data.Foldable                  (toList)
import           Data.Maybe                     (mapMaybe)
--
import           Data.Bitree                    (duplicate)
import           Data.BitreeZipper
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple.Convert                      (mkLemmaMap,lemmatize)
import           NLP.Type.PennTreebankII
--
import           SRL.Type
--


phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)


findNotOverlappedNodes :: PennTreeIdx -> Range -> [Range]
findNotOverlappedNodes ipt rng = filter (`isNotOverlappedWith` rng)
                               . map (\(PN (r,_) _) -> r)
                               . filter (\case PN _ _ -> True ; _ -> False)
                               . biList
                               . duplicate 
                               $ ipt 
  

isVBN :: BitreeZipperICP a -> Bool
isVBN z = case current z of
            PL (_,x) -> posTag x == VBN
            _        -> False 


withCopula :: BitreeZipperICP (a,Lemma) -> Bool
withCopula z = case current <$> (prev <=< parent) z of
                 Just (PL (_,x)) -> snd (getAnnot x) == "be"
                 _               -> False


isInNP :: BitreeZipperICP (a,Lemma) -> Bool
isInNP z = case current <$> (parent <=< parent) z of
             Just (PN (_,x) _) -> chunkTag x == NP
             _                 -> False


isInPP :: BitreeZipperICP (a,Lemma) -> Bool
isInPP z = case current <$> (parent z) of
             Just (PN (_,x) _) -> chunkTag x == PP
             _               -> False


isPassive :: BitreeZipperICP (a,Lemma) -> Bool
isPassive z = let b1 = isVBN z
                  b2 = withCopula z
                  b3 = isInNP z
                  b4 = isInPP z
              in (b1 && b2) || (b1 && b3) || (b1 && b4)


voice :: (PennTree,S.Sentence) -> [(Int,(Lemma,Voice))]
voice (pt,sent) = 
  let ipt = mkPennTreeIdxA (mkPennTreeIdx pt)
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,ALeaf (VBN,_) ((),lma)) -> Just (n,(lma,if isPassive z then Passive else Active))
                  _                                -> Nothing
  in mapMaybe testf $ toList (mkBitreeZipper [] lemmapt)

