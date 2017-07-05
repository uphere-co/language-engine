{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SRL.Feature.Verb where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable                               (toList)
import           Data.Maybe
import           Data.Monoid
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
import           Debug.Trace


phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)


getLeaf (PL (_,x)) = Just x
getLeaf _          = Nothing


getLeafIndex (PL (i,_)) = Just i
getLeafIndex _          = Nothing


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


getIdxPOS w = (,) <$> getLeafIndex w <*> fmap posTag (getLeaf w)

withCopula :: BitreeZipperICP (Lemma ': as) -> Maybe (Int,POSTag)
withCopula z = check1 z <|> check2 z
  where check1 z = do w <- current <$> (prev <=< parent) z
                      guard (isLemmaAs "be" w)
                      getIdxPOS w
        check2 z = do w <- current <$> (child1 <=< parent <=< parent) z
                      guard (isLemmaAs "be" w)
                      getIdxPOS w



withHave :: BitreeZipperICP (Lemma ': as) -> Maybe (Int,POSTag)
withHave z = check1 z <|> check2 z <|> check3 z
  where check1 z = do w <- current <$> (prev <=< parent) z
                      guard (isLemmaAs "have" w)
                      getIdxPOS w
        check2 z = do w <- current <$> (child1 <=< parent <=< parent) z
                      guard (isLemmaAs "have" w)
                      getIdxPOS w
        check3 z = do w <- current <$> (child1 <=< parent <=< parent <=< parent) z
                      guard (isLemmaAs "have" w)
                      getIdxPOS w



isInNP :: BitreeZipperICP as -> Bool
isInNP z = maybe False (isChunkAs NP . current) ((parent <=< parent) z)


isInPP :: BitreeZipperICP as -> Bool
isInPP z = maybe False (isChunkAs PP . current) (parent z)


isPassive :: BitreeZipperICP (Lemma ': as) -> Bool
isPassive z
  = let b1 = isVBN z
        b2 = isJust (withCopula z)
        b3 = isInNP z
        b4 = isInPP z
    in (b1 && b2) || (b1 && b3) || (b1 && b4)



verbProperty :: BitreeZipperICP (Lemma ': as) -> Maybe VerbProperty -- (Tense,Aspect,Voice,[Int])
verbProperty z = do
  i <- getLeafIndex (current z)
  lma <- ahead . getAnnot <$> getLeaf (current z)
  let b0 = isVBN z
      b1 = isVBG z
      m2 = withCopula z
      b2 = isJust m2
      m3 = withHave z
      b3 = isJust m3
      asp = if b0 && b3
            then Perfect
            else if (b1 && b2)
                 then if b3
                      then PerfectProgressive
                      else Progressive
                 else Simple
      vo  = if isPassive z then Passive else Active
      tns = case (asp,vo,m2,m3) of
              (Perfect,           _      ,_     ,Just p) -> if snd p == VBD then Past else Present
              (PerfectProgressive,_      ,_     ,Just p) -> if snd p == VBD then Past else Present
              (_                 ,Passive,Just p,_     ) -> if snd p == VBD then Past else Present
              _                                          -> Present
                
      is = catMaybes $
             (if asp == Perfect || asp == PerfectProgressive then [fmap fst m3] else []) <>
             (if asp == Progressive || asp == PerfectProgressive || vo == Passive then [fmap fst m2] else []) <>
             [Just i]
  return (VerbProperty i lma tns asp vo is)


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


getVerbProperty :: (PennTree,S.Sentence) -> [VerbProperty]
getVerbProperty (pt,sent) = 
  let ipt = mkAnnotatable (mkPennTreeIdx pt)
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,ALeaf (pos,_) annot)
                    -> if isVerb pos && ahead annot /= "be" && ahead annot /= "have"
                       then verbProperty z
                       else Nothing
                  _
                    -> Nothing 
  in mapMaybe testf $ toList (mkBitreeZipper [] lemmapt)

