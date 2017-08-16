{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module NLP.Syntax.Verb where

import           Control.Applicative
import           Control.Lens                                ((^.),(^..),_1,_2)
import           Control.Monad
import           Control.Monad.Loops                         (iterateUntilM)
import           Data.Foldable                               (toList)
import           Data.Function                               (on)
import           Data.IntMap                                 (IntMap)
import           Data.List                                   (sortBy)
import           Data.Text                                   (Text)
import           Data.Maybe
import           Data.Monoid
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import           CoreNLP.Simple.Convert                      (mkLemmaMap,lemmatize)
import           Data.Attribute
import           Data.Bitree
import           Data.BitreeZipper
import           NLP.Type.PennTreebankII
--
import           NLP.Syntax.Type
import           NLP.Syntax.Util


auxBe :: BitreeZipperICP (Lemma ': as) -> Maybe a -> Maybe a -> Maybe a -> Maybe a -> Maybe a
auxBe z fd fg fn f =
   if | isLemmaAs "be" (current z) && isPOSAs VBD (current z) -> fd
      | isLemmaAs "be" (current z) && isPOSAs VBG (current z) -> fg
      | isLemmaAs "be" (current z) && isPOSAs VBN (current z) -> fn
      | isLemmaAs "be" (current z)                            -> f
      | otherwise                                             -> Nothing  

auxHave :: BitreeZipperICP (Lemma ': as) -> Maybe Tense
auxHave z = 
  if | isLemmaAs "have" (current z) && isPOSAs VBD (current z) -> return Past
     | isLemmaAs "have" (current z)                            -> return Present
     | otherwise                                               -> Nothing



findPrevVerb :: BitreeZipperICP (Lemma ': as) -> Maybe (BitreeZipperICP (Lemma ': as))
findPrevVerb z = do
    p <- parent z
    ((prevVerbInSiblings p)
     <|>
     (do z1 <- parent p
         guard (isChunkAs VP (current z1))
         prevVerbInSiblings z1
     ))
  where
    prevVerbInSiblings = findSiblings prev (\x -> case getIdxPOS x of {Nothing -> False; Just (_,pos) -> isVerb pos})


findAux :: (GetIntLemma tag) =>
           BitreeZipperICP tag
        -> Maybe (BitreeZipperICP tag, (Int,Lemma))
findAux z = do
  p <- parent z
  guard (isChunkAs VP (current p))
  c <- child1 p
  if | isPOSAs MD (current c)     -> (c,) <$> intLemma c
     | isPOSAs TO (current c)     -> (c,) <$> intLemma c  
     | isLemmaAs "do" (current c) -> (c,) <$> intLemma c
     | otherwise                  -> findAux p


findNeg :: (GetIntLemma tag) => 
           BitreeZipperICP tag
        -> Maybe (BitreeZipperICP tag, (Int,Lemma))
findNeg z = (\z'->(z',) <$> intLemma z') =<< (findNegInSiblings prev z <|> findNegInSiblings next z)
  where
    findNegInSiblings dir = findSiblings dir (\x -> isPOSAs RB x && (isLemmaAs "not" x || isLemmaAs "n't" x))


auxNegWords
  :: (GetIntLemma tag) =>
     BitreeZipperICP tag
  -> [BitreeZipperICP tag]
  -> AuxNegWords (BitreeZipperICP tag)
auxNegWords z zs =
  let zis = map (\z'->(z',)<$>intLemma z') zs
      (au,ne) = case findAux z of
                  Nothing -> (Nothing,findNeg z)
                  Just (c,il) -> (Just (c,il),findNeg c)

      ws = sortBy (compare `on` (^._2._1)) (catMaybes ([au,ne] ++ zis))
  in (au,ne,ws)


tenseAspectVoiceAuxNeg
  :: BitreeZipperICP '[Lemma]
  -> Maybe (Tense,Aspect,Voice,AuxNegWords (BitreeZipperICP '[Lemma]))
tenseAspectVoiceAuxNeg z
  | isPOSAs VBN (current z) = 
      case findPrevVerb z of
        Nothing -> return (Present,Simple,Passive,auxNegWords z [z])
        Just z1 -> do
          ((auxBe z1
            (return (Past,Simple,Passive,auxNegWords z1 [z1,z]))
            (findPrevVerb z1 >>= \z2 -> 
              auxBe z2
                (return (Past,Progressive,Passive,auxNegWords z2 [z2,z1,z]))
                Nothing
                (findPrevVerb z2 >>= \z3 -> do
                   t <- auxHave z3
                   return (t,PerfectProgressive,Passive,auxNegWords z3 [z3,z2,z1,z]))
                (return (Present,Progressive,Passive,auxNegWords z2 [z2,z1,z])))
            (do z2 <- findPrevVerb z1
                t <- auxHave z2
                return (t,Perfect,Passive,auxNegWords z2 [z2,z1,z]))
            (return (Present,Simple,Passive,auxNegWords z1 [z1,z])))
           <|>
           (auxHave z1 >>= \t -> return (t,Perfect,Active,auxNegWords z1 [z1,z])))
  | isPOSAs VBG (current z) = do
      case findPrevVerb z of
        Nothing -> return (Present,Progressive,Active,auxNegWords z [z])
        Just z1 -> do
          z1 <- findPrevVerb z
          auxBe z1 (return (Past,Progressive,Active,auxNegWords z1 [z1,z]))
                   Nothing
                   (do z2 <- findPrevVerb z1
                       t <- auxHave z2
                       return (t,PerfectProgressive,Active,auxNegWords z2 [z2,z1,z]))
                   (return (Present,Progressive,Active,auxNegWords z1 [z1,z]))
  | isPOSAs VBD (current z) =    -- Penn Treebank POS Tagging often tags participles with past tense verbs.
      case findPrevVerb z of
        Nothing -> return (Past,Simple,Active,auxNegWords z [z])
        Just z1 -> do
          ((auxBe z1
            (return (Past,Simple,Passive,auxNegWords z1 [z1,z]))
            (findPrevVerb z1 >>= \z2 -> 
              auxBe z2
                (return (Past,Progressive,Passive,auxNegWords z2 [z2,z1,z]))
                Nothing
                (findPrevVerb z2 >>= \z3 -> do
                   t <- auxHave z3
                   return (t,PerfectProgressive,Passive,auxNegWords z3 [z3,z2,z1,z]))
                (return (Present,Progressive,Passive,auxNegWords z2 [z2,z1,z])))
            (do z2 <- findPrevVerb z1
                t <- auxHave z2
                return (t,Perfect,Passive,auxNegWords z2 [z2,z1,z]))
            (return (Present,Simple,Passive,auxNegWords z1 [z1,z])))
           <|>
           (auxHave z1 >>= \t -> return (t,Perfect,Active,auxNegWords z1 [z1,z])))

    -- return (Past,Simple,Active,auxNegWords z [z])
  | otherwise               = return (Present,Simple,Active,auxNegWords z [z])


verbProperty :: BitreeZipperICP '[Lemma] -> Maybe (VerbProperty (BitreeZipperICP '[Lemma]))
verbProperty z = do
  i <- getLeafIndex (current z)
  lma <- ahead . getAnnot <$> getLeaf (current z)
  (tns,asp,vo,(aux,neg,is)) <- tenseAspectVoiceAuxNeg z
  return (VerbProperty i lma tns asp vo aux neg is)


verbPropertyFromPennTree :: IntMap Lemma -> PennTree -> [VerbProperty (BitreeZipperICP '[Lemma])]
verbPropertyFromPennTree lemmamap pt = 
  let lemmapt = lemmatize lemmamap (mkAnnotatable (mkPennTreeIdx pt))
      phase1 z = case getRoot (current z) of
                  Right (_,ALeaf (pos,_) annot)
                    -> if isVerb pos && ahead annot /= "be" && ahead annot /= "have" && ahead annot /= "do"
                       then verbProperty z
                       else Nothing
                  _ -> Nothing 
      vps1 = mapMaybe phase1 (toList (mkBitreeZipper [] lemmapt))
      identified_verbs1 = concatMap (\vp -> vp^..(vp_words.traverse._2._1)) vps1
      
      phase2 z = case getRoot (current z) of
                  Right (i,ALeaf (pos,_) annot)
                    -> if isVerb pos && ahead annot /= "be" && ahead annot /= "have" && (not (i `elem` identified_verbs1))
                       then verbProperty z
                       else Nothing
                  _ -> Nothing
      vps2 = mapMaybe phase2 (toList (mkBitreeZipper [] lemmapt))
      identified_verbs2 = concatMap (\vp -> vp^..(vp_words.traverse._2._1)) vps2

      phase3 z = case getRoot (current z) of
                  Right (i,ALeaf (pos,_) _annot)
                    -> if isVerb pos && (not (i `elem` (identified_verbs1++identified_verbs2)))
                       then verbProperty z
                       else Nothing
                  _ -> Nothing
      vps3 = mapMaybe phase3 (toList (mkBitreeZipper [] lemmapt))

  in vps1 <> vps2 <> vps3



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


-- | modal verbs
--   https://en.wikipedia.org/wiki/English_modal_verbs
modalVerbs :: [ Text ]
modalVerbs = [ "can"
             , "could"
             , "may"
             , "might"
             , "shall"
             , "should"
             , "will"
             , "would"
             , "must"
             ]

semiModalVerbs :: [ Text ]
semiModalVerbs = [ "ought"
                 , "dare"
                 , "need"
                 , "had_better"
                 , "used_to"
                 ]


nonModalAuxiliary :: [ Text ]
nonModalAuxiliary = [ "be_going_to"
                    , "have_to"
                    , "do"
                    ]
