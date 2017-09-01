{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze.SentenceStructure where

import           Control.Lens                              ((^.),(^..),_1,to)
import           Data.Bifunctor                            (bimap)
import           Data.Function                             (on)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.List                                 (sortBy)
import           Data.Maybe                                (fromMaybe,maybeToList)
import           Data.Monoid                               ((<>))
import qualified Data.Text                         as T
import           Data.Text                                 (Text)
--
import           CoreNLP.Simple.Convert                    (mkLemmaMap')
import           FrameNet.Query.Frame                      (FrameDB,frameDB)
import           FrameNet.Type.Common                      (CoreType(..))
import           FrameNet.Type.Frame                       (fe_coreType,fe_name,frame_FE)
import           Lexicon.Type                              (POSVorN(..),GRel
                                                           ,RoleInstance,RolePattInstance
                                                           ,ArgPattern)
import           NLP.Syntax.Clause                         (bindingAnalysis,clauseStructure,identifyCPHierarchy)
import           NLP.Syntax.Verb                           (verbPropertyFromPennTree)
import           NLP.Syntax.Type.Verb                      (VerbProperty,vp_lemma)
import           NLP.Syntax.Type.XBar                      (Zipper)
import qualified NLP.Type.NamedEntity              as N
import           NLP.Type.CoreNLP                          (NERSentence(..),Token,Dependency,Sentence,SentenceIndex
                                                           ,sentenceNER,sentenceWord,sentenceToken,sentenceLemma)

import           NLP.Type.PennTreebankII                   (Lemma(..),PennTree,mkPennTreeIdx)
import qualified NLP.Type.PennTreebankII.Separated as PS
import           NLP.Type.SyntaxProperty                   (Voice)
import           WikiEL.EntityLinking                      (EntityMention)
--
import           OntoNotes.Type.SenseInventory
--
import           SRL.Analyze.Format                        (getTopPatternsFromONFNInst)
import           SRL.Analyze.Type
import           SRL.Analyze.Util                          (TagPos(..))
import           SRL.Analyze.WikiEL                        (getWikiResolvedMentions
                                                           ,linkedMentionToTagPos)


mergeTagPos :: (Ord i) => [TagPos i a] -> [TagPos i b] -> [TagPos i (Either a b)]
mergeTagPos xs ys =
  let zs = map (fmap Left) xs ++ map (fmap Right) ys
      idx (TagPos (i,_,_)) = i
  in sortBy (compare `on` idx) zs




getSenses :: Text
          -> HashMap Text Inventory
          -> HashMap (Text,Text) Int
          -> FrameDB
          -> HashMap Text [(Text,Text)]
          -> [(ONSenseFrameNetInstance,Int)]
getSenses lma sensemap sensestat framedb ontomap = do
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let sid = (lma,Verb, s^.sense_group <> "." <> s^.sense_n)
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt_def = T.take 40 (s^.sense_name)
      tframe = fromMaybe (Left FrameNone) $ do
        lst <- HM.lookup lma ontomap
        frtxt <- lookup (s^.sense_group <> "." <> s^.sense_n) lst
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


getTopPatternsFromSensesAndVP :: [RoleInstance]
                              -> [RolePattInstance Voice]
                              -> [(ONSenseFrameNetInstance,Int)]
                              -> ([(ONSenseFrameNetInstance,Int)]
                                 ,Maybe (RoleInstance,Maybe [(ArgPattern () GRel,Int)]))
getTopPatternsFromSensesAndVP rolemap subcats senses =
  let mrmmtoppatts = getTopPatternsFromONFNInst rolemap subcats =<< fmap (^._1) (chooseFrame senses)
  in (senses,mrmmtoppatts)


-- | Finding the structure of the sentence and formatting it.
--
docStructure :: AnalyzePredata
             -> ([(Text, N.NamedEntityClass)] -> [EntityMention Text])
             -> DocAnalysisInput
             -> DocStructure
docStructure apredata emTagger docinput@(DocAnalysisInput sents _sentidxs sentitems _tokss mptrs _deps mtmxs) =
  let lmass = sents ^.. traverse . sentenceLemma . to (map Lemma)
      mtokenss = sents ^.. traverse . sentenceToken
      linked_mentions_resolved = getWikiResolvedMentions emTagger
                                                         (docinput^.dainput_sents)
                                                         (concat (docinput^.dainput_tokss))
      lnk_mntns_tagpos = map linkedMentionToTagPos linked_mentions_resolved
      mkidx = zipWith (\i x -> fmap (i,) x) (cycle ['a'..'z'])
      mergedtags = maybe (map (fmap Left) lnk_mntns_tagpos) (mergeTagPos lnk_mntns_tagpos . mkidx) mtmxs
      sentStructures = map (sentStructure apredata) (zip3 ([0..]::[Int]) lmass mptrs)
  in DocStructure mtokenss sentitems mergedtags sentStructures



sentStructure :: AnalyzePredata -> (Int,[Lemma],Maybe PennTree) -> Maybe SentStructure
sentStructure apredata (i,lmas,mptr) =
  flip fmap mptr $ \ptr ->
    let lemmamap = (mkLemmaMap' . map unLemma) lmas
        vps = verbPropertyFromPennTree lemmamap ptr
        clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,PS.convert c)) id (mkPennTreeIdx ptr))
        mcpstr = (fmap (map bindingAnalysis) . identifyCPHierarchy) vps
        verbStructures = map (verbStructure apredata) vps
    in SentStructure i ptr vps clausetr mcpstr verbStructures


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
      mrmmtoppatts = getTopPatternsFromONFNInst rolemap subcats =<< fmap (^._1) (chooseFrame senses)
  in VerbStructure vp lma senses mrmmtoppatts
