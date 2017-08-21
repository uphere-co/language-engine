{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module OntoNotes.App.Analyze.SentenceStructure where

import           Control.Lens                              ((^.),(^..),to)
import           Data.Bifunctor                            (bimap)
import           Data.Foldable                             (forM_)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.List                                 (zip5)
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
import           Lexicon.Type                              (ArgPattern(..),RoleInstance,RolePattInstance)
import           NLP.Printer.PennTreebankII                (formatIndexTokensFromTree)
import           NLP.Type.PennTreebankII                   (PennTree)
import           NLP.Syntax.Clause                         (clauseStructure,constructCP)
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
import           OntoNotes.App.Analyze.CoreNLP             (runParser)
import           OntoNotes.App.Analyze.Format              (formatSenses,formatTimex
                                                           ,formatNER
                                                           ,showTimex,showFormatTimex'
                                                           )
import           OntoNotes.App.Util                        (CharIdx,SentItem,TagPos)
import           OntoNotes.App.WikiEL                      (getWikiResolvedMentions)
import           OntoNotes.Type.SenseInventory


mergeTimexWikiNER :: [(SentItem CharIdx, [TagPos CharIdx (Maybe Text)])]
                  -> [EntityMention Text]
                  -> Text 
mergeTimexWikiNER sentswithtmx linked_mentions_resolved =
  T.pack (show sentswithtmx) <> "\n" <> T.pack (show linked_mentions_resolved)

  


getSenses :: Text -> HashMap Text Inventory -> HashMap (Text,Text) Int -> FrameDB -> HashMap Text [(Text,Text)]
          -> [(Text,Text,Int,Text,Text,Text,Text)]
getSenses lma sensemap sensestat framedb ontomap = do
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt_def = T.take 40 (s^.sense_name)
      (txt_frame,txt_fecore,txt_feperi)
        = fromMaybe ("","","") $ do
            lst <- HM.lookup lma ontomap
            frtxt <- lookup (s^.sense_group <> "." <> s^.sense_n) lst
            case frtxt of
              "copula"    -> return ("** COPULA **"    , "","")
              "idioms"    -> return ("** IDIOMS **"    , "","")
              "lightverb" -> return ("** LIGHT VERB **", "","")
              _ -> do
                frame <- HM.lookup frtxt (framedb^.frameDB)
                let fes = frame^..frame_FE.traverse
                    corefes = filter (\fe -> fe^.fe_coreType == Core || fe^.fe_coreType == CoreUnexpressed) fes
                    perifes = filter (\fe -> fe^.fe_coreType == Peripheral) fes
                    fecoretxt = T.intercalate ", " (map (^.fe_name) corefes)
                    feperitxt = T.intercalate ", " (map (^.fe_name) perifes)
                return (frtxt,fecoretxt,feperitxt)
  return (s^.sense_group,s^.sense_n,num,txt_def,txt_frame,txt_fecore,txt_feperi)


-- | Finding the structure of the sentence and formatting it.
-- 
sentStructure :: HashMap Text Inventory
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
                 , Maybe [(SentItem CharIdx, [TagPos CharIdx (Maybe Text)])]
                 )
              -> [Text]
sentStructure sensemap sensestat framedb ontomap emTagger rolemap subcats loaded =
  let (sents,sentidxs,sentitems,_tokss,mptrs,deps,mtmx) = loaded
      lmass = sents ^.. traverse . sentenceLemma
      mtokenss = sents ^.. traverse . sentenceToken
      linked_mentions_resolved = getWikiResolvedMentions loaded emTagger
      line1 = [ "================================================================================================="
              , "-- TimeTagger -----------------------------------------------------------------------------------" ]
      line2 = case mtmx of
                Nothing -> ["Time annotation not successful!"]
                Just sentswithtmx -> [mergeTimexWikiNER sentswithtmx linked_mentions_resolved]
                                     -- concat $ map formatTimex sentswithtmx

      line3 = [ "-- WikiNamedEntityTagger ------------------------------------------------------------------------"
              , T.pack (render (formatNER mtokenss sentitems linked_mentions_resolved))
              , "--------------------------------------------------------------------------------------------------"
              , "-- Sentence analysis -----------------------------------------------------------------------------"
              , "--------------------------------------------------------------------------------------------------" ]

      mlines = flip map (zip3 ([0..] :: [Int]) lmass mptrs) $ \(i,lmas,mptr) ->
                 flip fmap mptr $ \ptr ->
                   let lemmamap = mkLemmaMap' lmas
                       vps = verbPropertyFromPennTree lemmamap ptr
                       clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,PS.convert c)) id (mkPennTreeIdx ptr))

                       subline1 = [ T.pack (printf "-- Sentence %3d ----------------------------------------------------------------------------------" i)
                                  , formatIndexTokensFromTree 0 ptr
                                  , "--------------------------------------------------------------------------------------------------"
                                  , formatClauseStructure vps clausetr
                                  , "================================================================================================="
                                  ]

                       subline2 = flip map (vps^..traverse) $ \vp ->
                                    let mcp = constructCP vp
                                        lma = vp^.vp_lemma.to unLemma
                                        senses = getSenses lma sensemap sensestat framedb ontomap
                                        ssubline1 = [ formatVPwithPAWS clausetr vp
                                                    , T.pack (printf "Verb: %-20s" lma)
                                                    , T.pack $ (formatSenses False rolemap subcats lma) senses
                                                    ]
                                    in ssubline1
                   in (subline1, subline2)
      line4 = concatMap f mlines
        where f mxs = case mxs of
                        Nothing       -> [""]
                        Just (xs,yss) -> xs ++ (concat yss)

  in line1 ++ line2 ++ line3 ++ line4
