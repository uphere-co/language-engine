{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module OntoNotes.App.Analyze.SentenceStructure where

import           Control.Lens                              ((^.),(^..),_1,to)
import           Data.Bifunctor                            (bimap)
import           Data.Foldable                             (forM_)
import           Data.Function                             (on)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.List                                 (sortBy,zip5)
import           Data.Maybe                                (fromMaybe,maybeToList)
import           Data.Monoid                               ((<>))
import qualified Data.Text                         as T
import           Data.Text                                 (Text)
import qualified Data.Text.IO                      as T.IO
import           Data.Traversable                          (forM)
import qualified Language.Java                     as J
import           Text.PrettyPrint.Boxes                    (render)
import           Text.Printf                               (printf)
--
import           CoreNLP.Simple.Convert                    (mkLemmaMap,mkLemmaMap',sentToNER')
import           CoreNLP.Simple.Type.Simplified            (NERSentence(..),Token,Dependency,Sentence,SentenceIndex
                                                           ,sentenceNER,sentenceWord,sentenceToken,sentenceLemma)
import           FrameNet.Query.Frame                      (FrameDB,frameDB)
import           FrameNet.Type.Common                      (CoreType(..))
import           FrameNet.Type.Frame                       (fe_coreType,fe_name,frame_FE)
import           Lexicon.Type                              (ArgPattern(..),POSVorN(..),RoleInstance,RolePattInstance)
import           NLP.Printer.PennTreebankII                (formatIndexTokensFromTree)
import           NLP.Type.PennTreebankII                   (PennTree)
import           NLP.Syntax.Clause                         (clauseStructure,constructCP
                                                           ,identifyCPHierarchy
                                                           )
import           NLP.Syntax.Format                         (formatCP,formatVPwithPAWS
                                                           ,formatClauseStructure,showClauseStructure)
import           NLP.Syntax.Verb                           (verbPropertyFromPennTree)
import           NLP.Syntax.Type                           (Voice,vp_lemma)
import qualified NLP.Type.NamedEntity              as N
import           NLP.Type.PennTreebankII                   (Lemma(..),mkPennTreeIdx)
import qualified NLP.Type.PennTreebankII.Separated as PS
import           WikiEL.EntityLinking                      (EntityMentionUID,EntityMention(..),UIDCite(..)
                                                           ,entityLinking,entityLinkings,buildEntityMentions,entityUID)
import           WikiEL.WikiNamedEntityTagger              (PreNE(..))
--
import           OntoNotes.App.Util                        (CharIdx,SentItem,TagPos(..),TokIdx)
import           OntoNotes.App.WikiEL                      (getWikiResolvedMentions
                                                           ,linkedMentionToTagPos
                                                           )
import           OntoNotes.Type.SenseInventory
--
import qualified OntoNotes.App.Analyze.Config   as Analyze (Config, showDetail)
import           OntoNotes.App.Analyze.CoreNLP             (runParser)
import           OntoNotes.App.Analyze.Format              (formatSenses,formatTimex
                                                           ,formatTagged
                                                           ,showTimex,showFormatTimex'
                                                           ,getTopPatternsFromONFNInst
                                                           )
import           OntoNotes.App.Analyze.Type


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


-- | Finding the structure of the sentence and formatting it.
--
sentStructure :: Analyze.Config
              -> HashMap Text Inventory
              -> HashMap (Text, Text) Int
              -> FrameDB
              -> HashMap Text [(Text, Text)]
              -> ([(Text, N.NamedEntityClass)] -> [EntityMention Text])
              -> [RoleInstance]
              -> [RolePattInstance Voice]
              -> ( [Sentence]
                 , [Maybe SentenceIndex]
                 , [SentItem CharIdx]
                 , [[Token]]
                 , [Maybe PennTree]
                 , [Dependency]
                 , Maybe [TagPos TokIdx (Maybe Text)]
                 )
              -> [Text]
sentStructure config sensemap sensestat framedb ontomap emTagger rolemap subcats loaded =
  let (sents,sentidxs,sentitems,_tokss,mptrs,deps,mtmxs) = loaded
      lmass = sents ^.. traverse . sentenceLemma
      mtokenss = sents ^.. traverse . sentenceToken
      linked_mentions_resolved = getWikiResolvedMentions loaded emTagger
      lnk_mntns_tagpos = map linkedMentionToTagPos linked_mentions_resolved
      mkidx = zipWith (\i x -> fmap (i,) x) (cycle ['a'..'z'])
      mergedtags = maybe (map (fmap Left) lnk_mntns_tagpos) (mergeTagPos lnk_mntns_tagpos . mkidx) mtmxs
      line1 = [ "==================================================================================================" ]
      line2 = [ "-- Time and NER tagged text ----------------------------------------------------------------------"
              , T.pack (render (formatTagged mtokenss sentitems mergedtags))
              , "--------------------------------------------------------------------------------------------------"
              , "-- Sentence analysis -----------------------------------------------------------------------------"
              , "--------------------------------------------------------------------------------------------------" ]

      mlines = flip map (zip3 ([0..] :: [Int]) lmass mptrs) $ \(i,lmas,mptr) ->
                 flip fmap mptr $ \ptr ->
                   let lemmamap = mkLemmaMap' lmas
                       vps = verbPropertyFromPennTree lemmamap ptr
                       clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,PS.convert c)) id (mkPennTreeIdx ptr))
                       mcpstr = identifyCPHierarchy vps

                       subline1 = [ T.pack (printf "-- Sentence %3d ----------------------------------------------------------------------------------" i)
                                  , formatIndexTokensFromTree 0 ptr
                                  ]

                       subline1_1 = [ "--------------------------------------------------------------------------------------------------"
                                    , formatClauseStructure vps clausetr
                                    , "================================================================================================="
                                    ]

                       subline2 = flip map (vps^..traverse) $ \vp ->
                                    let mcp = constructCP vp
                                        lma = vp^.vp_lemma.to unLemma
                                        senses = getSenses lma sensemap sensestat framedb ontomap
                                        mrmmtoppatts = getTopPatternsFromONFNInst rolemap subcats =<<
                                                       fmap (^._1) (chooseFrame senses)
                                    in [ formatVPwithPAWS clausetr mcpstr vp
                                       , T.pack (printf "Verb: %-20s" lma)
                                       , T.pack $ (formatSenses False rolemap subcats lma senses mrmmtoppatts)
                                       ]
                   in (subline1 ++ if config^.Analyze.showDetail then subline1_1 else [], subline2)
      line3 = concatMap f mlines
        where f mxs = case mxs of
                        Nothing       -> [""]
                        Just (xs,yss) -> xs ++ (concat yss)

  in line1 ++ line2 ++ line3
