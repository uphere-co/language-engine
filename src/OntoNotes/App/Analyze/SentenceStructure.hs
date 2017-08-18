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
import           NLP.Printer.PennTreebankII                (formatIndexTokensFromTree)
import           NLP.Type.PennTreebankII                   (PennTree)
import           NLP.Syntax.Clause                         (clauseStructure,constructCP)
import           NLP.Syntax.Format                         (formatCP,formatClauseStructure,showClauseStructure)
import           NLP.Syntax.Verb                           (verbPropertyFromPennTree)
import           NLP.Syntax.Type                           (vp_lemma)
import qualified NLP.Type.NamedEntity              as N
import           NLP.Type.PennTreebankII                   (Lemma(..),mkPennTreeIdx)
import qualified NLP.Type.PennTreebankII.Separated as PS
import           WikiEL.EntityLinking                      (EntityMentionUID,EntityMention(..),UIDCite(..)
                                                           ,entityLinking,entityLinkings,buildEntityMentions,entityUID)
import           WikiEL.WikiNamedEntityTagger              (PreNE(..))                 
--
import           OntoNotes.App.Analyze.CoreNLP             (runParser)
import           OntoNotes.App.Analyze.Format              (formatSenses,formatTimex
                                                           ,formatNER,formatNER'
                                                           ,showTimex,showFormatTimex'
                                                           )
import           OntoNotes.App.Util                        (SentItem,TagPos)
import           OntoNotes.Type.ArgTable
import           OntoNotes.Type.SenseInventory



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





sentStructure :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
              -> HashMap Text Inventory
              -> HashMap (Text,Text) Int
              -> FrameDB
              -> HashMap Text [(Text,Text)]
              -> ([(Text,N.NamedEntityClass)] -> [EntityMention Text])
              -> [((Text,Text), [(Text,Text)])]
              -> [((Text,Text),[(ArgPattern Text,Int)])]
              -> Text
              -> IO ()
sentStructure pp sensemap sensestat framedb ontomap emTagger rolemap subcats txt = do
  (psents,sents,sentitems,_tokss,mptrs,deps,mtmx,linked_mentions_resolved) <- runParser pp emTagger txt
  putStrLn "\n\n\n\n\n\n\n\n================================================================================================="
  putStrLn "\n\n-- TimeTagger -----------------------------------------------------------------------------------"
  case mtmx of
    Nothing -> putStrLn "Time annotation not successful!"
    Just sentswithtmx -> mapM_ showTimex sentswithtmx
  putStrLn "-- WikiNamedEntityTagger ------------------------------------------------------------------------"
  putStrLn (render (formatNER psents sentitems linked_mentions_resolved))
  putStrLn "\n\n--------------------------------------------------------------------------------------------------"
  putStrLn "-- Sentence analysis -----------------------------------------------------------------------------"
  putStrLn "--------------------------------------------------------------------------------------------------"

  flip mapM_ (zip5 ([0..] :: [Int]) psents sents mptrs deps) $ \(i,psent,_sent,mptr,_dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let lemmamap = mkLemmaMap psent
          vps = verbPropertyFromPennTree lemmamap ptr

      putStrLn (printf "\n\n-- Sentence %3d ----------------------------------------------------------------------------------" i)
      T.IO.putStrLn (formatIndexTokensFromTree 0 ptr)
      
      putStrLn "--------------------------------------------------------------------------------------------------"
      showClauseStructure lemmamap ptr
      putStrLn "================================================================================================="

      forM_ (vps^..traverse) $ \vp -> do
        let mcp = constructCP vp
        let lma = vp^.vp_lemma.to unLemma
        putStrLn (printf "Verb: %-20s" lma)
        let senses = getSenses lma sensemap sensestat framedb ontomap
        (putStrLn . formatSenses False rolemap subcats lma) senses
        -- putStrLn "--------------------------------------------------------------------------------------------------"
        putStrLn (maybe "cannot identify CP" formatCP mcp)
        putStrLn "--------------------------------------------------------------------------------------------------"



sentStructure' :: HashMap Text Inventory
               -> HashMap (Text, Text) Int
               -> FrameDB
               -> HashMap Text [(Text, Text)]
               -> ([(Text,N.NamedEntityClass)] -> [EntityMention Text])
               -> [((Text,Text), [(Text,Text)])]
               -> [((Text,Text),[(ArgPattern Text,Int)])]
               -> ([Sentence], [Maybe SentenceIndex], [SentItem], [[Token]], [Maybe PennTree], [Dependency], Maybe [ (SentItem, [TagPos (Maybe Text)]) ] )
               -> IO ()
sentStructure' sensemap sensestat framedb ontomap emTagger rolemap subcats loaded = do
  let (all,sents,sentitems,_tokss,mptrs,deps,mtmx) = loaded
  let psents = all ^.. traverse . sentenceLemma
      mtokens = all ^.. traverse . sentenceToken
      mws = all ^.. traverse . sentenceWord
      mns = all ^.. traverse . sentenceNER
  let unNER (NERSentence tokens) = tokens
      neTokens = concat $ map (\(x,y) -> (unNER $ sentToNER' x y)) (zip mws mns) 
      linked_mentions_all = emTagger neTokens
      linked_mentions_resolved
        = filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all


  putStrLn "\n\n\n\n\n\n\n\n================================================================================================="
  putStrLn "\n\n-- TimeTagger -----------------------------------------------------------------------------------"
  case mtmx of
    Nothing -> putStrLn "Time annotation not successful!"
    Just sentswithtmx -> mapM_ showFormatTimex' sentswithtmx
  putStrLn "-- WikiNamedEntityTagger ------------------------------------------------------------------------"
  putStrLn (render (formatNER' mtokens sentitems linked_mentions_resolved))
  putStrLn "\n\n--------------------------------------------------------------------------------------------------"
  putStrLn "-- Sentence analysis -----------------------------------------------------------------------------"
  putStrLn "--------------------------------------------------------------------------------------------------"

  flip mapM_ (zip5 ([0..] :: [Int]) psents sents mptrs deps) $ \(i,psent,_sent,mptr,_dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let lemmamap = mkLemmaMap' psent
          vps = verbPropertyFromPennTree lemmamap ptr

      putStrLn (printf "\n\n-- Sentence %3d ----------------------------------------------------------------------------------" i)
      T.IO.putStrLn (formatIndexTokensFromTree 0 ptr)
      
      putStrLn "--------------------------------------------------------------------------------------------------"
      showClauseStructure lemmamap ptr
      putStrLn "================================================================================================="

      forM_ (vps^..traverse.vp_lemma.to unLemma) $ \lma -> do
        putStrLn (printf "Verb: %-20s" lma)
        let senses = getSenses lma sensemap sensestat framedb ontomap
        (putStrLn . formatSenses False rolemap subcats lma) senses
        putStrLn "--------------------------------------------------------------------------------------------------"







getSentStructure :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
                 -> HashMap Text Inventory
                 -> HashMap (Text, Text) Int
                 -> FrameDB
                 -> HashMap Text [(Text, Text)]
                 -> ([(Text, N.NamedEntityClass)] -> [EntityMention Text])
                 -> [((Text,Text), [(Text,Text)])]
                 -> [((Text,Text),[(ArgPattern Text,Int)])]
                 -> Text
                 -> IO [Text]
getSentStructure pp sensemap sensestat framedb ontomap emTagger rolemap subcats txt = do
  (psents,sents,sentitems,_tokss,mptrs,deps,mtmx,linked_mentions_resolved) <- runParser pp emTagger txt

  let line1 = [ "================================================================================================="
              , "-- TimeTagger -----------------------------------------------------------------------------------" ]

  let line2 = case mtmx of
                Nothing -> ["Time annotation not successful!"]
                Just sentswithtmx -> concat $ map formatTimex sentswithtmx

  let line3 = [ "-- WikiNamedEntityTagger ------------------------------------------------------------------------"
              , T.pack (render (formatNER psents sentitems linked_mentions_resolved))
              , "--------------------------------------------------------------------------------------------------"
              , "-- Sentence analysis -----------------------------------------------------------------------------"
              , "--------------------------------------------------------------------------------------------------" ]
              
  mlines <- flip mapM (zip5 ([0..] :: [Int]) psents sents mptrs deps) $ \(i,psent,_sent,mptr,_dep) -> do
    flip mapM mptr $ \ptr -> do
      let lemmamap = mkLemmaMap psent
          vps = verbPropertyFromPennTree lemmamap ptr
          clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,PS.convert c)) id (mkPennTreeIdx ptr))

      let subline1 = concat [ [T.pack (printf "-- Sentence %3d ----------------------------------------------------------------------------------" i)]
                     , [(formatIndexTokensFromTree 0 ptr)]
                     , ["--------------------------------------------------------------------------------------------------"]
                     , formatClauseStructure vps clausetr
                     , ["================================================================================================="] ] 

      subline2 <- forM (vps^..traverse.vp_lemma.to unLemma) $ \lma -> do
        let senses = getSenses lma sensemap sensestat framedb ontomap
        let ssubline1 = [ T.pack (printf "Verb: %-20s" lma)
                        , T.pack $ (formatSenses False rolemap subcats lma) senses
                        , "--------------------------------------------------------------------------------------------------" ]
        return ssubline1
      return (subline1, subline2)
  let line4 = concat $ map f mlines
        where f mxs = case mxs of
                        Nothing       -> [""]
                        Just (xs,yss) -> xs ++ (concat yss)

  return $ line1 ++ line2 ++ line3 ++ line4
