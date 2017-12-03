{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module SRL.Analyze.SentenceStructure where

import           Control.Lens                              ((^.),(^..),_1,_2,_3,to)
import           Data.Bifunctor                            (first)
import           Data.Either                               (lefts)
import           Data.Foldable                             (toList)
import           Data.List                                 (zip5)
import           Data.Maybe                                (fromMaybe,mapMaybe)
import           Data.Monoid                               ((<>))
import qualified Data.Text                         as T
import           Data.Text                                 (Text)
--
import           CoreNLP.Simple.Convert                    (mkLemmaMap)
import           NLP.Syntax.Clause                         (bindingAnalysis,bindingAnalysisRaising,identifyCPHierarchy,resolveCP)
import           NLP.Syntax.Verb                           (verbPropertyFromPennTree)
import           NLP.Syntax.Type                           (MarkType(..))
import           NLP.Syntax.Type.Verb                      (VerbProperty,vp_lemma)
import           NLP.Syntax.Type.XBar                      (Zipper)
import           NLP.Syntax.Util                           (mkTaggedLemma)
import qualified NLP.Type.NamedEntity              as N
import           NLP.Type.CoreNLP                          (Sentence,SentenceIndex,Token,sentenceToken,sentenceLemma,sent_tokenRange)
import           NLP.Type.PennTreebankII                   (Lemma(..),PennTree)
import           NLP.Type.TagPos                           (TagPos(..),TokIdx(..),mergeTagPos)
import           WikiEL.EntityLinking                      (entityPreNE,entityName)
import           WikiEL.Type                               (EntityMention,IRange(..),PreNE(..),UIDCite(..))
import           WikiEL.WikiEntityClass                    (orgClass,personClass,brandClass)
import           WordNet.Type.Lexicographer                (LexicographerFile)
--

import           SRL.Analyze.Sense                         (getVerbSenses)
import           SRL.Analyze.Type
import           SRL.Analyze.UKB                           (runUKB)


adjustWikiRange :: (Int,Int) -> (Int,Int)
adjustWikiRange (a,b) = (a,b-1)


linkedMentionToTagPos :: (EntityMention Text)
                      -> (TagPos TokIdx (EntityMention Text))
linkedMentionToTagPos linked_mention =
  let IRange b e = (_info linked_mention)^._1
  in TagPos (TokIdx b, TokIdx e,linked_mention)


mkWikiList :: SentStructure -> [((Int, Int), Text)]
mkWikiList sstr =
  let tagposs = sstr ^.. ss_tagged_full . traverse . to (fmap fst)
      wikiel  = lefts $ map (\(TagPos (i,j,e)) -> first (unTokIdx i,unTokIdx j,) e) tagposs
      wikilst = mapMaybe  (\(i,j,w) -> ((i,j-1),) <$> getNEFunc w) wikiel
      getNEFunc e =
        case (_info e)^._3 of
          UnresolvedUID x    -> Just (entityName (_info e) <> "(" <> T.pack (show x) <> ")" )
          AmbiguousUID (_,x) -> Just (entityName (_info e) <> "(" <> T.pack (show x)<> ")" )
          Resolved (i,c)     -> Just (entityName (_info e) <> "(" <> T.pack (show c) <> "," <> T.pack (show i) <> ")")
          UnresolvedClass _  -> Nothing
  in wikilst



nerDocument :: AnalyzePredata
          -> ([Sentence] -> [EntityMention Text])
          -> DocAnalysisInput
          -> [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text))] -- IO DocStructure
nerDocument apredata netagger docinput@(DocAnalysisInput sents sentidxs sentitems _ mptrs _ mtmxs) =
  let lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
      mtokenss = sents ^.. traverse . sentenceToken
      linked_mentions_resolved = netagger (docinput^.dainput_sents)
      lnk_mntns_tagpos = map linkedMentionToTagPos linked_mentions_resolved
      mkidx = zipWith (\i x -> fmap (i,) x) (cycle ['a'..'z'])
  in maybe (map (fmap Left) lnk_mntns_tagpos) (mergeTagPos lnk_mntns_tagpos . mkidx) mtmxs


--
-- | Finding the structure of the sentence and formatting it.
--
docStructure :: AnalyzePredata
             -> ([Sentence] -> [EntityMention Text])
             -> DocAnalysisInput
             -> IO DocStructure
docStructure apredata netagger docinput@(DocAnalysisInput sents sentidxs sentitems _ mptrs _ mtmxs) = do
  let lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
      mtokenss = sents ^.. traverse . sentenceToken  
      mergedtags = nerDocument apredata netagger docinput
  synsetss <- runUKB (apredata^.analyze_wordnet)(sents,mptrs)
  let sentStructures = map (sentStructure apredata mergedtags) (zip5 ([1..] :: [Int]) sentidxs lmass mptrs synsetss)
  return (DocStructure mtokenss sentitems mergedtags sentStructures)


tagToMark :: Either (EntityMention Text) (Char,Maybe Text) -> Maybe MarkType
tagToMark (Right _) = Just MarkTime -- time is special
tagToMark (Left x)  = case entityPreNE x of
                        Resolved (_,c) ->
                          if | c == orgClass    -> Just (MarkEntity N.Org)
                             | c == personClass -> Just (MarkEntity N.Person)
                             | c == brandClass  -> Just (MarkEntity N.Other)
                             | otherwise        -> Nothing
                        UnresolvedUID c ->
                          if c `elem` [N.Org,N.Person]
                          then Just (MarkEntity c)
                          else Nothing
                        AmbiguousUID (_,c) ->
                          if c `elem` [N.Org,N.Person]
                          then Just (MarkEntity c)
                          else Nothing
                        _ -> Nothing


adjustTokenIndexForSentence :: Maybe SentenceIndex
                            -> [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text))]
                            -> [TagPos TokIdx (Either (EntityMention Text) (Char, Maybe Text), MarkType)]
adjustTokenIndexForSentence midx tagged
  = fromMaybe [] $ do
      (b0,e0) <- (^.sent_tokenRange) <$> midx
      return $ flip mapMaybe tagged $ \(TagPos (TokIdx b,TokIdx e,t)) -> do
        t' <- tagToMark t
        if b0 <= b && e <= e0
          then return (TagPos (TokIdx (b-b0),TokIdx (e-b0),(t,t')))
          else Nothing



sentStructure :: AnalyzePredata
              -> [TagPos TokIdx (Either (EntityMention Text) (Char,Maybe Text))]
              -> (Int,Maybe SentenceIndex,[Lemma],Maybe PennTree,[(Int,LexicographerFile)])
              -> Maybe SentStructure
sentStructure apredata taglst (i,midx,lmas,mptr,synsets) =
  flip fmap mptr $ \ptr -> 
    let taglst' = adjustTokenIndexForSentence midx taglst
        taglstMarkOnly = map (fmap snd) taglst'
        lmatkns = (zip [0..] . zip lmas . map (^._2) . toList) ptr
        lemmamap = (mkLemmaMap . map unLemma) lmas
        taggedMarkOnly = mkTaggedLemma lmatkns ptr taglstMarkOnly synsets
        vps = verbPropertyFromPennTree lemmamap ptr
        x'tr = (map (bindingAnalysisRaising . resolveCP . bindingAnalysis taggedMarkOnly) . identifyCPHierarchy taggedMarkOnly) vps
        verbStructures = map (verbStructure apredata) vps
    in SentStructure i ptr vps x'tr taglst' taggedMarkOnly verbStructures


verbStructure :: AnalyzePredata -> VerbProperty (Zipper '[Lemma]) -> VerbStructure
verbStructure apredata vp =
  let (senses,rmtoppatts) = getVerbSenses apredata (vp^.vp_lemma)
  in VerbStructure vp senses rmtoppatts
