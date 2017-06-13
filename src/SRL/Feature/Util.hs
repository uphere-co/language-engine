{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SRL.Feature.Util where

import           Control.Lens            hiding (levels,Level)
import           Control.Monad                  ((<=<),guard)
import           Data.Bifunctor                 (bimap)
import           Data.Bifoldable                (biList)
import           Data.Foldable                  (toList)
import           Data.Function                  (on)
import           Data.Graph                     (buildG,dfs)
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap             as IM
import           Data.List                      (foldl',group,sortBy)
import           Data.Maybe                     (catMaybes,fromJust,mapMaybe)
import           Data.Text                      (Text)
import           Data.Tree                      (levels)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple.Convert                      (cutf8)
import           CoreNLP.Simple.Type.Simplified
import           NLP.Type.PennTreebankII
import           NLP.Type.TreeZipper
import           PropBank.Type.Prop
--
import           SRL.Format
import           SRL.PropBankMatch
import           SRL.Type
import           SRL.Util
--


phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)


mkLemmaMap :: S.Sentence -> IntMap Text
mkLemmaMap sent = foldl' (\(!acc) (k,v) -> IM.insert k v acc) IM.empty $
                    zip [0..] (catMaybes (sent ^.. S.token . traverse . TK.lemma . to (fmap cutf8)))

lemmatize :: IntMap Text
          -> PennTreeIdxG ChunkTag (POSTag,Text)
          -> PennTreeIdxG ChunkTag (POSTag,(Text,Text))
lemmatize m = bimap id (\(i,(p,x)) -> (i,(p,(x,fromJust (IM.lookup i m)))))

findNotOverlappedNodes :: PennTreeIdx -> Range -> [Range]
findNotOverlappedNodes ipt rng = filter (`isNotOverlappedWith` rng)
                               . map (\(PN (r,_) _) -> r)
                               . filter (\case PN _ _ -> True ; _ -> False)
                               . biList
                               . duplicate 
                               $ ipt 
  

isVBN :: TreeZipperICP a -> Bool
isVBN z = case current z of
            PL (_,(p,_)) -> p == VBN
            _            -> False 

withCopula :: TreeZipperICP (Text,Text) -> Bool
withCopula z = case current <$> (prev <=< parent) z of
                 Just (PL (_,(_,(_,l)))) -> l == "be"
                 _                       -> False

isInNP :: TreeZipperICP (Text,Text) -> Bool
isInNP z = case current <$> (parent <=< parent) z of
             Just (PN (_,c) _) -> c == NP
             _             -> False


isInPP :: TreeZipperICP (Text,Text) -> Bool
isInPP z = case current <$> (parent z) of
             Just (PN (_,c) _) -> c == PP
             _                 -> False

isPassive :: TreeZipperICP (Text,Text) -> Bool
isPassive z = let b1 = isVBN z
                  b2 = withCopula z
                  b3 = isInNP z
                  b4 = isInPP z
              in (b1 && b2) || (b1 && b3) || (b1 && b4)


voice :: (PennTree,S.Sentence) -> [(Int,(Text,Voice))]
voice (pt,sent) = 
  let ipt = mkPennTreeIdx pt
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      getf (PL x) = Right x
      getf (PN x _) = Left x
      testf z = case getf (current z) of
                  Right (n,(VBN,(txt,_))) -> Just (n,(txt,if isPassive z then Passive else Active))
                  _ -> Nothing
  in mapMaybe testf $ toList (mkTreeZipper [] lemmapt)

