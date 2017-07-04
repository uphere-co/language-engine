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


isLemmaAs lma (PL (_,x)) = ahead (getAnnot x) == lma
isLemmaAs _   _          = False


isPOSAs pos (PL (_,x)) = posTag x == pos
isPOSAs _   _          = False


isChunkAs chk (PN (_,x) _) = chunkTag x == chk
isChunkAs _   _            = False



isVBN :: BitreeZipperICP a -> Bool
isVBN z = isPOSAs VBN (current z)


isVBG :: BitreeZipperICP a -> Bool
isVBG z = isPOSAs VBG (current z)


withCopula :: BitreeZipperICP (Lemma ': as) -> Bool
withCopula z = check1 z || check2 z
  where check1 z = maybe False (isLemmaAs "be" . current) ((prev <=< parent) z)
        check2 z = maybe False (isLemmaAs "be" . current) ((child1 <=< parent <=< parent) z)   -- case for "it's not done" 



withHave :: BitreeZipperICP (Lemma ': as) -> Bool
withHave z = check1 z
  where check1 z = maybe False (isLemmaAs "have" . current) ((prev <=< parent) z)
        check2 z = maybe False (isLemmaAs "have" . current) ((child1 <=< parent <=< parent) z)   -- case for "it's not done"     


isInNP :: BitreeZipperICP as -> Bool
isInNP z = maybe False (isChunkAs NP . current) ((parent <=< parent) z)


isInPP :: BitreeZipperICP as -> Bool
isInPP z = maybe False (isChunkAs PP . current) (parent z)


isPassive :: BitreeZipperICP (Lemma ': as) -> Bool
isPassive z
  = let b1 = isVBN z
        b2 = withCopula z
        b3 = isInNP z
        b4 = isInPP z
    in (b1 && b2) || (b1 && b3) || (b1 && b4)


verbProperty :: BitreeZipperICP (Lemma ': as) -> (Aspect,Voice)
verbProperty z
  = let b0 = isVBN z
        b1 = isVBG z
        b2 = withCopula z
        b3 = withHave z
        asp = if b0 && b3
              then Perfect
              else if (b1 && b2)
                   then if b3
                        then PerfectProgressive
                        else Progressive
                   else Simple
        vo  = if isPassive z then Passive else Active
    in (asp,vo)



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


getVerbProperty :: (PennTree,S.Sentence) -> [(Int,(Lemma,(Aspect,Voice)))]
getVerbProperty (pt,sent) = 
  let ipt = mkAnnotatable (mkPennTreeIdx pt)
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,ALeaf (pos,_) annot)
                    -> if isVerb pos && ahead annot /= "be" && ahead annot /= "have"
                       then Just (n,(ahead annot,verbProperty z))
                       else Nothing
                  _
                    -> Nothing 
  in mapMaybe testf $ toList (mkBitreeZipper [] lemmapt)

