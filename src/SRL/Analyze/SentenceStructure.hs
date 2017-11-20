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
import           Data.Function                             (on)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.List                                 (find,sortBy,zip4)
import           Data.Maybe                                (fromMaybe,mapMaybe,maybeToList)
import           Data.Monoid                               ((<>))
import qualified Data.Text                         as T
import           Data.Text                                 (Text)
--
import           CoreNLP.Simple.Convert                    (mkLemmaMap')
import           FrameNet.Query.Frame                      (FrameDB,frameDB)
import           FrameNet.Type.Common                      (CoreType(..))
import           FrameNet.Type.Frame                       (fe_coreType,fe_name,frame_FE)
import           Lexicon.Merge                             (constructTopPatterns)
import           Lexicon.Query                             (cutHistogram)
import           Lexicon.Type                              (POSVorN(..),FNFrame(..),GRel
                                                           ,RoleInstance,RolePattInstance
                                                           ,ArgPattern)
import           NLP.Syntax.Clause                         (bindingAnalysis,bindingAnalysisRaising,identifyCPHierarchy,resolveCP)
import           NLP.Syntax.Verb                           (verbPropertyFromPennTree)
import           NLP.Syntax.Type                           (MarkType(..))
import           NLP.Syntax.Type.Verb                      (VerbProperty,vp_lemma)
import           NLP.Syntax.Type.XBar                      (Zipper)
import           NLP.Syntax.Util                           (mkTaggedLemma)
import qualified NLP.Type.NamedEntity              as N
import           NLP.Type.CoreNLP                          (Sentence,SentenceIndex,sentenceToken,sentenceLemma,sent_tokenRange)

import           NLP.Type.PennTreebankII                   (Lemma(..),PennTree)
import           NLP.Type.SyntaxProperty                   (Voice)
import           NLP.Type.TagPos                           (TagPos(..),TokIdx(..),mergeTagPos)
import           OntoNotes.Type.SenseInventory
import           WikiEL.EntityLinking                      (entityPreNE,entityName)
import           WikiEL.Type                               (EntityMention,IRange(..),PreNE(..),UIDCite(..))
import           WikiEL.WikiEntityClass                    (orgClass,personClass,brandClass)
--
import           SRL.Analyze.Parameter                     (thresholdPattStat)
import           SRL.Analyze.Type


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


getSenses :: Text
          -> HashMap Text Inventory
          -> HashMap (Text,Text) Int
          -> FrameDB
          -> HashMap Text [(Text,FNFrame)]
          -> [(ONSenseFrameNetInstance,Int)]
getSenses lma sensemap sensestat framedb ontomap = do
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let sid = (lma,Verb, s^.sense_group <> "." <> s^.sense_n)
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt_def = s^.sense_name
      tframe = fromMaybe (Left FrameNone) $ do
        lst <- HM.lookup lma ontomap
        frtxt <- unFNFrame <$> lookup (s^.sense_group <> "." <> s^.sense_n) lst
        case frtxt of
          "copula"    -> return (Left FrameCopula)
          "idioms"    -> return (Left FrameIdiom)
          "lightverb" -> return (Left FrameLightVerb)
          _ -> do
            frame <- HM.lookup frtxt (framedb^.frameDB)
            let fes = frame^..frame_FE.traverse
                corefes = map (^.fe_name)
                        . filter (\fe -> fe^.fe_coreType == Core || fe^.fe_coreType == CoreUnexpressed)
                        $ fes
                perifes = map (^.fe_name)
                        . filter (\fe -> fe^.fe_coreType == Peripheral)
                        $ fes
            return (Right (TF frtxt corefes perifes))
  return ((ONFNInstance sid txt_def tframe),num)



getTopPatternsFromONFNInst :: [RoleInstance]
                           -> [RolePattInstance Voice]
                           -> (ONSenseFrameNetInstance,Int)
                           -> [((RoleInstance,Int), [(ArgPattern () GRel,Int)])]
getTopPatternsFromONFNInst rolemap subcats (inst,n) = do
  let sid = inst^.onfn_senseID
  rm <- filter (\rm -> rm^._1 == sid) rolemap
  let subcats' = maybeToList (find ((== sid) . (^._1)) subcats)
      toppatts_cut = cutHistogram thresholdPattStat . constructTopPatterns . (^._2) =<< subcats'
  return ((rm,n),toppatts_cut)


-- | Finding the structure of the sentence and formatting it.
--
docStructure :: AnalyzePredata
             -> ([Sentence] -> [EntityMention Text])
             -> DocAnalysisInput
             -> DocStructure
docStructure apredata netagger docinput@(DocAnalysisInput sents sentidxs sentitems _ mptrs _ mtmxs) =
  let lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
      mtokenss = sents ^.. traverse . sentenceToken
      linked_mentions_resolved = netagger (docinput^.dainput_sents)
      lnk_mntns_tagpos = map linkedMentionToTagPos linked_mentions_resolved
      mkidx = zipWith (\i x -> fmap (i,) x) (cycle ['a'..'z'])
      mergedtags = maybe (map (fmap Left) lnk_mntns_tagpos) (mergeTagPos lnk_mntns_tagpos . mkidx) mtmxs
      sentStructures = map (sentStructure apredata mergedtags) (zip4 ([1..] :: [Int]) sentidxs lmass mptrs)
  in DocStructure mtokenss sentitems mergedtags sentStructures


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
              -> (Int,Maybe SentenceIndex,[Lemma],Maybe PennTree)
              -> Maybe SentStructure
sentStructure apredata taglst (i,midx,lmas,mptr) =
  flip fmap mptr $ \ptr ->
    let taglst' = adjustTokenIndexForSentence midx taglst
        taglstMarkOnly = map (fmap snd) taglst'
        lmatkns = (zip [0..] . zip lmas . map (^._2) . toList) ptr
        lemmamap = (mkLemmaMap' . map unLemma) lmas
        taggedMarkOnly = mkTaggedLemma lmatkns ptr taglstMarkOnly        
        vps = verbPropertyFromPennTree lemmamap ptr
        x'tr = (map (bindingAnalysisRaising . resolveCP . bindingAnalysis taggedMarkOnly) . identifyCPHierarchy taggedMarkOnly) vps
        verbStructures = map (verbStructure apredata) vps
    in SentStructure i ptr vps x'tr taglst' taggedMarkOnly verbStructures


verbStructure :: AnalyzePredata -> VerbProperty (Zipper '[Lemma]) -> VerbStructure
verbStructure apredata vp =
  let lma = vp^.vp_lemma.to unLemma
      sensemap = apredata^.analyze_sensemap
      sensestat = apredata^.analyze_sensestat
      framedb = apredata^.analyze_framedb
      ontomap = apredata^.analyze_ontomap
      rolemap = apredata^.analyze_rolemap
      subcats = apredata^.analyze_subcats

      senses = getSenses lma sensemap sensestat framedb ontomap
      rmtoppatts = do inst <- sortBy (flip compare `on` (^._2)) senses
                      getTopPatternsFromONFNInst rolemap subcats inst
  in VerbStructure vp senses rmtoppatts
