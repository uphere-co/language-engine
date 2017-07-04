{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SRL.Feature.Verb where

import           Control.Monad                               ((<=<))
import           Data.Foldable                               (toList)
import           Data.Maybe                                  (mapMaybe)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple.Convert                      (mkLemmaMap,lemmatize)
import           Data.Attribute
import           Data.Bitree                                 (duplicate)
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
--
import           SRL.Type
--


phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)


isVBN :: BitreeZipperICP a -> Bool
isVBN z = case current z of
            PL (_,x) -> posTag x == VBN
            _        -> False 


isVBG :: BitreeZipperICP a -> Bool
isVBG z = case current z of
            PL (_,x) -> posTag x == VBG
            _        -> False 


withCopula :: BitreeZipperICP (Lemma ': as) -> Bool
withCopula z = check1 z || check2 z
  where becheck (Just (PL (_,x))) = ahead (getAnnot x) == "be"
        becheck _                 = False

        check1 z = becheck (current <$> (prev <=< parent) z)
        check2 z = becheck (current <$> (child1 <=< parent <=< parent) z)   -- case for "it's not done" 
         


isInNP :: BitreeZipperICP as -> Bool
isInNP z = case current <$> (parent <=< parent) z of
             Just (PN (_,x) _) -> chunkTag x == NP
             _                 -> False


isInPP :: BitreeZipperICP as -> Bool
isInPP z = case current <$> (parent z) of
             Just (PN (_,x) _) -> chunkTag x == PP
             _               -> False


isPassive :: BitreeZipperICP (Lemma ': as) -> Bool
isPassive z
  = let b1 = isVBN z
        b2 = withCopula z
        b3 = isInNP z
        b4 = isInPP z
    in (b1 && b2) || (b1 && b3) || (b1 && b4)


determineAspect :: BitreeZipperICP (Lemma ': as) -> Aspect
determineAspect z
  = let b1 = isVBG z
        b2 = withCopula z
    in if (b1 && b2) then Progressive else Simple



voice :: (PennTree,S.Sentence) -> [(Int,(Lemma,Voice))]
voice (pt,sent) = 
  let ipt = mkAnnotatable (mkPennTreeIdx pt)
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,ALeaf (VBN,_) annot)
                    -> Just (n,(ahead annot,if isPassive z then Passive else Active))
                  _
                    -> Nothing
  in mapMaybe testf $ toList (mkBitreeZipper [] lemmapt)


aspect :: (PennTree,S.Sentence) -> [(Int,(Lemma,Aspect))]
aspect (pt,sent) = 
  let ipt = mkAnnotatable (mkPennTreeIdx pt)
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,ALeaf (VBG,_) annot)
                    -> Just (n,(ahead annot,determineAspect z))
                  _
                    -> Nothing
  in mapMaybe testf $ toList (mkBitreeZipper [] lemmapt)

