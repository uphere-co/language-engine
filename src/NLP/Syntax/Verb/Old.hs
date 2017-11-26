{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- This module has old ad hoc voice analyses, which are still used in
-- semantic-role-labeler at this moment.
-- most up-to-date tense,aspect,voice analysis is in NLP.Syntax.Verb.

module NLP.Syntax.Verb.Old where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Foldable                               (toList)
import           Data.Maybe
--
import           CoreNLP.Proto.CoreNLPProtos.Sentence as S
import           CoreNLP.Simple.Convert                      (mkLemmaMapFromPSent,lemmatize)
import           Data.Attribute                              (ahead)
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty                     (Voice(..))
--
import           NLP.Syntax.Type.XBar
import           NLP.Syntax.Util


withCopula :: Zipper (Lemma ': as) -> Maybe (Int,POSTag)
withCopula x = do p1 <- parent x
                  p2 <- (parent <=< parent) x
                  p3 <- (parent <=< parent <=< parent) x
                  check1 p1 x <|> check2 p1 p2 x <|> check3 p1 p2 p3 x
  where check1 p1 z = do
          w <- current <$> (prev <=< parent) z
          guard (isChunkAs VP (current p1))
          guard (isLemmaAs "be" w)
          getIdxPOS w
        check2 p1 p2 z = do
          w <- current <$> (child1 <=< parent <=< parent) z
          guard (isChunkAs VP (current p1))
          guard (isChunkAs VP (current p2))
          guard (isLemmaAs "be" w)
          getIdxPOS w
        check3 p1 p2 p3 z = do
          w <- current <$> (child1 <=< parent <=< parent <=< parent) z
          guard (isChunkAs VP (current p1))
          guard (isChunkAs VP (current p2))
          guard (isChunkAs VP (current p3))
          guard (isLemmaAs "be" w)
          getIdxPOS w



withHave :: Zipper (Lemma ': as) -> Maybe (Int,POSTag)
withHave x = check1 x <|> check2 x <|> check3 x
  where check1 z = do w <- current <$> (prev <=< parent) z
                      guard (isLemmaAs "have" w)
                      getIdxPOS w
        check2 z = do w <- current <$> (child1 <=< parent <=< parent) z
                      guard (isLemmaAs "have" w)
                      getIdxPOS w
        check3 z = do w <- current <$> (child1 <=< parent <=< parent <=< parent) z
                      guard (isLemmaAs "have" w)
                      getIdxPOS w


isInNP :: Zipper as -> Bool
isInNP z = maybe False (isChunkAs NP . current) ((parent <=< parent) z)


isInPP :: Zipper as -> Bool
isInPP z = maybe False (isChunkAs PP . current) (parent z)


isPassive :: Zipper (Lemma ': as) -> Bool
isPassive z
  = let b1 = isVBN z
        b2 = isJust (withCopula z)
        b3 = isInNP z
        b4 = isInPP z
    in (b1 && b2) || (b1 && b3) || (b1 && b4)


                      
voice :: (PennTree,S.Sentence) -> [(Int,(Lemma,Voice))]
voice (pt,sent) = 
  let ipt = mkAnnotatable (mkPennTreeIdx pt)
      lemmamap = mkLemmaMapFromPSent sent
      lemmapt = lemmatize lemmamap ipt
      testf z = case getRoot (current z) of
                  Right (n,ALeaf (VBN,_) annot)
                    -> Just (n,(ahead annot,if isPassive z then Passive else Active))
                  _
                    -> Nothing
  in mapMaybe testf $ toList (mkBitreeZipper [] lemmapt)
