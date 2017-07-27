{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module NLP.Syntax.Verb where

import           Control.Applicative
import           Control.Lens                                ((^.))
import           Control.Monad
import           Control.Monad.Loops                         (iterateUntilM,unfoldWhileM)
import           Data.Foldable                               (toList)
import           Data.IntMap                                 (IntMap)
import           Data.Maybe
import           Data.Monoid
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple.Convert                      (mkLemmaMap,lemmatize)
import           Data.Attribute
import           Data.Bitree                                 (getRoot)
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Type
--
import           Debug.Trace

type BitreeICP lst = Bitree (Range,(ANAtt '[])) (Int,(ALAtt lst)) 

type BitreeZipperICP lst = BitreeZipper (Range,(ANAtt '[])) (Int,(ALAtt lst)) 


phraseType :: PennTreeIdxG c (p,a) -> (Range,Either c p)
phraseType (PN (i,c) _)   = (i,Left c)
phraseType (PL (n,(p,_))) = ((n,n),Right p)


getLeaf :: Bitree c (i,t) -> Maybe t
getLeaf (PL (_,x)) = Just x
getLeaf _          = Nothing


getLeafIndex :: Bitree c (i,t) -> Maybe i
getLeafIndex (PL (i,_)) = Just i
getLeafIndex _          = Nothing


isLemmaAs :: Lemma -> BitreeICP (Lemma ': as) -> Bool
isLemmaAs lma (PL (_,x)) = ahead (getAnnot x) == lma
isLemmaAs _   _          = False


isPOSAs :: POSTag -> BitreeICP as -> Bool
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


withCopula :: BitreeZipperICP (Lemma ': as) -> Maybe (Int,POSTag)
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



withHave :: BitreeZipperICP (Lemma ': as) -> Maybe (Int,POSTag)
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


findAux :: BitreeZipperICP (Lemma ': as) -> Maybe (Int,Lemma)
findAux z = do
  p <- parent z
  guard (isChunkAs VP (current p))
  c <- child1 p
  if isPOSAs MD (current c)
    then do i <- getLeafIndex (current c)
            l <- ahead . getAnnot <$> getLeaf (current c)
            return (i,l)
    else findAux p
          
          

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



findPrevVP :: BitreeZipperICP (Lemma ': as) -> Maybe (BitreeZipperICP (Lemma ': as))
findPrevVP z = do
  p <- parent z
  iterateUntilM (\z' -> case getIdxPOS (current z') of {Nothing -> False; Just (_,p) -> isVerb p}) prev p
  --listToMaybe lst


auxBe z fd fg fn f =
   if | isLemmaAs "be" (current z) && isPOSAs VBD (current z) -> fd
      | isLemmaAs "be" (current z) && isPOSAs VBG (current z) -> fg
      | isLemmaAs "be" (current z) && isPOSAs VBN (current z) -> fn
      | isLemmaAs "be" (current z)                            -> f
      | otherwise                                             -> Nothing  


auxHave z = 
  if | isLemmaAs "have" (current z) && isPOSAs VBD (current z) -> return Past
     | isLemmaAs "have" (current z)                            -> return Present
     | otherwise                                               -> Nothing

  
tenseAspectVoice :: BitreeZipperICP (Lemma ': as) -> Maybe (Tense,Aspect,Voice,[Int])
tenseAspectVoice z
  | isPOSAs VBN (current z) = do
      z1 <- findPrevVP z -- (child1 <=< parent <=< parent) z
      ((auxBe z1
        (return (Past,Simple,Passive,[getIdx z1,getIdx z]))
        (findPrevVP z1 >>= \z2 -> 
          auxBe z2
            (return (Past,Progressive,Passive,map getIdx [z2,z1,z]))
            Nothing
            (findPrevVP z2 >>= \z3 -> do
               t <- auxHave z3
               return (t,PerfectProgressive,Passive,map getIdx [z3,z2,z1,z]))
            (return (Present,Progressive,Passive,[getIdx z2,getIdx z1,getIdx z])))
        (do z2 <- findPrevVP z1
            t <- auxHave z2
            return (t,Perfect,Passive,map getIdx [z2,z1,z])
        )
        (return (Present,Simple,Passive,[getIdx z1,getIdx z])))
       <|>
       (if |isLemmaAs "have" (current z1) && isPOSAs VBD (current z1)
             -> return (Past,Perfect,Active,[getIdx z1,getIdx z])
           | isLemmaAs "have" (current z1) 
             -> return (Present,Perfect,Active,[getIdx z1,getIdx z])
           | otherwise
             -> Nothing))
  | isPOSAs VBG (current z) = do
      z1 <- findPrevVP z
      auxBe z1 (return (Past,Progressive,Active,[getIdx z1,getIdx z]))
               Nothing
               (do z2 <- findPrevVP z1
                   t <- auxHave z2
                   return (t,PerfectProgressive,Active,map getIdx [z2,z1,z]))
               (return (Present,Progressive,Active,[getIdx z1,getIdx z]))
  | isPOSAs VBD (current z) = return (Past,Simple,Active,[getIdx z])
  | otherwise               = return (Present,Simple,Active,[getIdx z])
  where getIdx = fst . fromJust . getIdxPOS . current 


verbProperty :: BitreeZipperICP (Lemma ': as) -> Maybe VerbProperty -- (Tense,Aspect,Voice,[Int])
verbProperty z = do
  i <- getLeafIndex (current z)
  lma <- ahead . getAnnot <$> getLeaf (current z)
  pos <- posTag <$> getLeaf (current z)
  {- let b0 = isVBN z
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
              _                                          -> if pos == VBD then Past else Present -}
  (tns,asp,vo,is) <- tenseAspectVoice z
  let aux = findAux z
      {- is = catMaybes $
             [fmap fst aux] <>
             (if asp == Perfect || asp == PerfectProgressive
              then [fmap fst m3]
              else []) <>
             (if asp == Progressive || asp == PerfectProgressive || vo == Passive
              then [fmap fst m2]
              else []) <>
             [Just i] -}
  return (VerbProperty i lma tns asp vo aux is)


voice :: (PennTree,S.Sentence) -> [(Int,(Lemma,Voice))]
voice (pt,sent) = 
  let ipt = mkAnnotatable (mkPennTreeIdx pt)
      lemmamap = mkLemmaMap sent
      lemmapt = lemmatize lemmamap ipt
      testf z = case getRoot (current z) of
                  Right (n,ALeaf (VBN,_) annot)
                    -> Just (n,(ahead annot,if isPassive z then Passive else Active))
                  _
                    -> Nothing
  in mapMaybe testf $ toList (mkBitreeZipper [] lemmapt)


verbPropertyFromPennTree :: IntMap Lemma -> PennTree -> [VerbProperty]
verbPropertyFromPennTree lemmamap pt = 
  let lemmapt = lemmatize lemmamap (mkAnnotatable (mkPennTreeIdx pt))
      phase1 z = case getRoot (current z) of
                  Right (_,ALeaf (pos,_) annot)
                    -> if isVerb pos && ahead annot /= "be" && ahead annot /= "have"
                       then verbProperty z
                       else Nothing
                  _ -> Nothing 
      vps1 = mapMaybe phase1 (toList (mkBitreeZipper [] lemmapt))
      identified_verbs = concatMap (\vp -> vp^.vp_words) vps1
      
      phase2 z = case getRoot (current z) of
                  Right (i,ALeaf (pos,_) _annot)
                    -> if isVerb pos && (not (i `elem` identified_verbs))
                       then verbProperty z
                       else Nothing
                  _ -> Nothing
      vps2 = mapMaybe phase2 (toList (mkBitreeZipper [] lemmapt))
  in vps1 <> vps2


