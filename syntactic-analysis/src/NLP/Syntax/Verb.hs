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
import           Control.Lens                                ((^.),(^..),(^?),_1,_2,_Just)
import           Control.Monad
import           Data.Foldable                               (toList)
import           Data.Function                               (on)
import           Data.IntMap                                 (IntMap)
import           Data.List                                   (sortBy)
import           Data.Text                                   (Text)
import           Data.Maybe
--
import           Data.Attribute
import           Data.Bitree
import           Data.BitreeZipper
import           Data.BitreeZipper.Util
import           NLP.Type.PennTreebankII
import           NLP.Type.SyntaxProperty                     (Tense(..),Voice(..),Aspect(..))
--
import           NLP.Syntax.Type.Verb
import           NLP.Syntax.Type.XBar
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
    prevVerbInSiblings = firstSiblingBy prev (\x -> case getIdxPOS x of {Nothing -> False; Just (_,pos) -> isVerb pos})


findMWAux :: (GetIntLemma tag) =>
             Bool
          -> BitreeZipperICP tag
          -> [(BitreeZipperICP tag, (Int,Lemma))]
findMWAux withto z
  | withto = fromMaybe [] $ do
               -- z is to.
               p   <- parent z
               guard (isChunkAs VP (current p) && isNothing (prev p))
               p'  <- parent p
               guard (isChunkAs S (current p'))
               p'' <- parent p'
               guard (isChunkAs VP (current p'') && isNothing ((prev <=< prev) p'))
               c   <- child1 p''
               let i = current c
               ((do guard (isLemmaAs "have" i || isLemmaAs "ought" i || (isLemmaAs "use" i && isPOSAs VBD i))
                    return $ mapMaybe (\x -> (x,) <$> intLemma x) [c,z])
                <|>
                (do guard (isLemmaAs "go" i && isPOSAs VBG i)
                    p''' <- parent p''
                    c' <- child1 p'''
                    guard (isLemmaAs "be" (current c'))
                    return $ mapMaybe (\x -> (x,) <$> intLemma x) [c',c,z]))
  | otherwise      = (z,) <$> maybeToList (intLemma z)   -- not implemented yet


findAux :: (GetIntLemma tag) =>
           Lemma
        -> BitreeZipperICP tag
        -> [(BitreeZipperICP tag, (Int,Lemma))]
findAux lma z = do
  p <- maybeToList (parent z)
  guard (isChunkAs VP (current p))
  c <- maybeToList (child1 p)
  if | isPOSAs MD (current c)     -> (c,) <$> maybeToList (intLemma c)
     | isPOSAs TO (current c)     -> let au = findMWAux True c
                                     in if null au then ((c,) <$> maybeToList (intLemma c)) else au
     | isLemmaAs "do" (current c) && unLemma lma /= "do" -> (c,) <$> maybeToList (intLemma c)
     | otherwise                  -> findAux lma p


findNeg :: (GetIntLemma tag) =>
           BitreeZipperICP tag
        -> Maybe (BitreeZipperICP tag, (Int,Lemma))
findNeg z = (\z'->(z',) <$> intLemma z') =<< (findNegInSiblings prev z <|> findNegInSiblings next z)
  where
    findNegInSiblings dir = firstSiblingBy dir (\x -> isPOSAs RB x && (isLemmaAs "not" x || isLemmaAs "n't" x))


auxNegWords
  :: (GetIntLemma tag) =>
     Lemma
  -> BitreeZipperICP tag
  -> [BitreeZipperICP tag]
  -> AuxNegWords (BitreeZipperICP tag)
auxNegWords lma z zs =
  let zis = map (\z'->(z',)<$>intLemma z') zs
      au = findAux lma z
      ne = case au of
             []        -> findNeg z
             (c,_il):_ -> findNeg c
      ws = sortBy (compare `on` (^._2._1)) (au ++ catMaybes (ne:zis))
  in (au,ne,ws)


tenseAspectVoiceAuxNeg
  :: BitreeZipperICP '[Lemma]
  -> Maybe (Tense,Aspect,Voice,AuxNegWords (BitreeZipperICP '[Lemma]))
tenseAspectVoiceAuxNeg z = do
    lma <- (intLemma z)^?_Just._2
    if | isPOSAs VBN (current z) -> vbn lma
       | isPOSAs VBG (current z) -> vbg lma
       | isPOSAs VBD (current z) -> vbd lma -- Penn Treebank POS Tagging often tags participles with past tense verbs.
       | otherwise               -> oth lma
 where
  vbn lma = case findPrevVerb z of
              Nothing -> return (Present,Simple,Passive,auxNegWords lma z [z])
              Just z1 -> do
                ((auxBe z1
                  (return (Past,Simple,Passive,auxNegWords lma z1 [z1,z]))
                  (findPrevVerb z1 >>= \z2 ->
                    auxBe z2
                      (return (Past,Progressive,Passive,auxNegWords lma z2 [z2,z1,z]))
                      Nothing
                      (findPrevVerb z2 >>= \z3 -> do
                         t <- auxHave z3
                         return (t,PerfectProgressive,Passive,auxNegWords lma z3 [z3,z2,z1,z]))
                      (return (Present,Progressive,Passive,auxNegWords lma z2 [z2,z1,z])))
                  (do z2 <- findPrevVerb z1
                      t <- auxHave z2
                      return (t,Perfect,Passive,auxNegWords lma z2 [z2,z1,z]))
                  (return (Present,Simple,Passive,auxNegWords lma z1 [z1,z])))
                 <|>
                 (auxHave z1 >>= \t -> return (t,Perfect,Active,auxNegWords lma z1 [z1,z])))
  --
  vbg lma = case findPrevVerb z of
              Nothing -> return (Present,Progressive,Active,auxNegWords lma z [z])
              Just z1 ->
                auxBe z1 (return (Past,Progressive,Active,auxNegWords lma z1 [z1,z]))
                         Nothing
                         (do z2 <- findPrevVerb z1
                             t <- auxHave z2
                             return (t,PerfectProgressive,Active,auxNegWords lma z2 [z2,z1,z]))
                         (return (Present,Progressive,Active,auxNegWords lma z1 [z1,z]))
  --
  vbd lma = case findPrevVerb z of
              Nothing -> return (Past,Simple,Active,auxNegWords lma z [z])
              Just z1 -> do
                ((auxBe z1
                  (return (Past,Simple,Passive,auxNegWords lma z1 [z1,z]))
                  (findPrevVerb z1 >>= \z2 ->
                    auxBe z2
                      (return (Past,Progressive,Passive,auxNegWords lma z2 [z2,z1,z]))
                      Nothing
                      (findPrevVerb z2 >>= \z3 -> do
                         t <- auxHave z3
                         return (t,PerfectProgressive,Passive,auxNegWords lma z3 [z3,z2,z1,z]))
                      (return (Present,Progressive,Passive,auxNegWords lma z2 [z2,z1,z])))
                  (do z2 <- findPrevVerb z1
                      t <- auxHave z2
                      return (t,Perfect,Passive,auxNegWords lma z2 [z2,z1,z]))
                  (return (Present,Simple,Passive,auxNegWords lma z1 [z1,z])))
                 <|>
                 (auxHave z1 >>= \t -> return (t,Perfect,Active,auxNegWords lma z1 [z1,z])))
  --
  oth lma = return (Present,Simple,Active,auxNegWords lma z [z])


verbProperty :: BitreeZipperICP '[Lemma] -> Maybe (VerbProperty (BitreeZipperICP '[Lemma]))
verbProperty z = do
  i <- getLeafIndex (current z)
  lma <- ahead . getAnnot <$> getLeaf (current z)
  (tns,asp,vo,(aux,neg,is)) <- tenseAspectVoiceAuxNeg z
  return (VerbProperty i lma tns asp vo aux neg is)


verbPropertyFromPennTree :: IntMap Lemma -> PennTree -> [VerbProperty (BitreeZipperICP '[Lemma])]
verbPropertyFromPennTree lemmamap pt =
  let lemmapt = mkBitreeICP lemmamap pt
      --
      phase1 z = case getRoot (current z) of
                  Right (_,ALeaf (pos,_) annot)
                    -> if isVerb pos && ahead annot /= "be" &&
                                        ahead annot /= "have" &&
                                        ahead annot /= "do" &&
                                        ahead annot /= "go" &&
                                        ahead annot /= "use"
                       then verbProperty z
                       else Nothing
                  _ -> Nothing
      vps1 = mapMaybe phase1 (toList (mkBitreeZipper [] lemmapt))
      identified_verbs1 = concatMap (\vp -> vp^..(vp_words.traverse._2._1)) vps1
      --
      phase2 z = case getRoot (current z) of
                  Right (i,ALeaf (pos,_) annot)
                    -> if isVerb pos && ahead annot /= "be" && ahead annot /= "have" && (not (i `elem` identified_verbs1))
                       then verbProperty z
                       else Nothing
                  _ -> Nothing
      vps2 = mapMaybe phase2 (toList (mkBitreeZipper [] lemmapt))
      identified_verbs2 = concatMap (\vp -> vp^..(vp_words.traverse._2._1)) vps2
      --
      phase3 z = case getRoot (current z) of
                  Right (i,ALeaf (pos,_) _annot)
                    -> if isVerb pos && (not (i `elem` (identified_verbs1++identified_verbs2)))
                       then verbProperty z
                       else Nothing
                  _ -> Nothing
      vps3 = mapMaybe phase3 (toList (mkBitreeZipper [] lemmapt))
      --
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
