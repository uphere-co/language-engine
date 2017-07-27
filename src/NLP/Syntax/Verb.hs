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
import           Data.Text                                   (Text)
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


findPrevVerb :: BitreeZipperICP (Lemma ': as) -> Maybe (BitreeZipperICP (Lemma ': as))
findPrevVerb z = do
    p <- parent z
    (prevVerbInSiblings p <|> (parent p >>= prevVerbInSiblings))
  where
    prevVerbInSiblings x = iterateUntilM (\z' -> case getIdxPOS (current z') of {Nothing -> False; Just (_,pos) -> isVerb pos}) prev x


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
      z1 <- findPrevVerb z
      ((auxBe z1
        (return (Past,Simple,Passive,[getIdx z1,getIdx z]))
        (findPrevVerb z1 >>= \z2 -> 
          auxBe z2
            (return (Past,Progressive,Passive,map getIdx [z2,z1,z]))
            Nothing
            (findPrevVerb z2 >>= \z3 -> do
               t <- auxHave z3
               return (t,PerfectProgressive,Passive,map getIdx [z3,z2,z1,z]))
            (return (Present,Progressive,Passive,[getIdx z2,getIdx z1,getIdx z])))
        (do z2 <- findPrevVerb z1
            t <- auxHave z2
            return (t,Perfect,Passive,map getIdx [z2,z1,z])
        )
        (return (Present,Simple,Passive,[getIdx z1,getIdx z])))
       <|>
       (auxHave z1 >>= \t -> return (t,Perfect,Active,map getIdx [z1,z])))
  | isPOSAs VBG (current z) = do
      z1 <- findPrevVerb z
      auxBe z1 (return (Past,Progressive,Active,[getIdx z1,getIdx z]))
               Nothing
               (do z2 <- findPrevVerb z1
                   t <- auxHave z2
                   return (t,PerfectProgressive,Active,map getIdx [z2,z1,z]))
               (return (Present,Progressive,Active,[getIdx z1,getIdx z]))
  | isPOSAs VBD (current z) = return (Past,Simple,Active,[getIdx z])
  | otherwise               = return (Present,Simple,Active,[getIdx z])
  where getIdx = fst . fromJust . getIdxPOS . current 


verbProperty :: BitreeZipperICP (Lemma ': as) -> Maybe VerbProperty
verbProperty z = do
  i <- getLeafIndex (current z)
  lma <- ahead . getAnnot <$> getLeaf (current z)
  pos <- posTag <$> getLeaf (current z)
  (tns,asp,vo,is) <- tenseAspectVoice z
  let aux = findAux z
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



-- | excerpted from https://en.wiktionary.org/wiki/Category:English_control_verbs 
controlVerbs :: [Text]
controlVerbs =
  [ "allow"
  , "ask"
  , "attempt"
  , "bother"
  , "cause"
  , "command"
  , "compel"
  , "connive"
  , "constrain"
  , "contrive"
  , "convince"
  , "demand"
  , "desire"
  , "endeavor"
  , "fail"
  , "help"
  , "hope"
  , "incentivize"
  , "long"
  , "make"
  , "manage"
  , "oblige"
  , "order"
  , "permit"
  , "persuade"
  , "plan"
  , "plot"
  , "proceed"
  , "require"
  , "start"
  , "strain"
  , "strive"
  , "struggle"
  , "tell"
  , "try"
  , "wait"
  , "want"
  , "wish"
  , "would"
  , "yearn"
  ]


-- | https://en.wiktionary.org/wiki/Category:English_copulative_verbs
--   This class of verbs seems closely related to subject raising.
copulativeVerbs :: [Text]
copulativeVerbs =
  [ "act"
  , "appear"
  , "arrive"
  , "be"
  , "become"
  , "bleed"
  , "break"
  , "come"
  , "emerge"
  , "fall"
  , "feel"
  , "get"
  , "grow"
  , "keep"
  , "look"
  , "play"
  , "prove"
  , "remain"
  , "run"
  , "seem"
  , "sound"
  , "test"
  ] 

{-
 -- from werdy
  copulas.add("act");
		copulas.add("appear");
		copulas.add("be");
		copulas.add("become");
		copulas.add("come");
		copulas.add("come out");
		copulas.add("end up");
		copulas.add("get");
		copulas.add("go");
		copulas.add("grow");
		copulas.add("fall");
		copulas.add("feel");
		copulas.add("keep");
		copulas.add("leave");
		copulas.add("look");
		copulas.add("prove");
		copulas.add("remain");
		copulas.add("seem");
		copulas.add("smell");
		copulas.add("sound");
		copulas.add("stay");
		copulas.add("taste");
		copulas.add("turn");
		copulas.add("turn up");
		copulas.add("wind up");
		copulas.add("have");
-}
