{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module OntoNotes.App.Analyze where

import           Control.Lens          hiding (Level)
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Foldable
import           Data.Function                    (on)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List                        (intercalate,intersperse,mapAccumL,zip5)
import           Data.Maybe
import           Data.Map                         (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Time.Calendar               (fromGregorian)
import           Data.Vector                      (Vector)
import qualified Data.Vector                as V
import           Language.Java              as J
import           System.Console.Haskeline
import           System.FilePath
import           System.Environment
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
import           Text.ProtocolBuffers.Basic       (Utf8)
import           Text.ProtocolBuffers.WireMessage (messageGet)
--
import qualified CoreNLP.Proto.CoreNLPProtos.Document  as D
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Timex     as Tmx
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import qualified CoreNLP.Proto.HCoreNLPProto.ListTimex as T
import qualified CoreNLP.Proto.HCoreNLPProto.TimexWithOffset as T
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           FrameNet.Query.Frame
import           FrameNet.Type.Common
import           FrameNet.Type.Frame             hiding (LexUnit)
import           NLP.Printer.PennTreebankII
import           NLP.Syntax.Clause
import           NLP.Syntax.Format
import           NLP.Syntax.Type
import           NLP.Syntax.Verb
import           NLP.Type.PennTreebankII
import           NLP.Type.PennTreebankII.Separated     as N
import           PropBank.Util
import           Text.Annotation.Type
import           Text.Annotation.Util.Doc
import           Text.Annotation.View


import           WikiEL                                       (loadEMtagger)
import           WikiEL.CoreNLP                               (parseNEROutputStr)
import           WikiEL.WikiEntityTagger                      (loadWETagger,wikiAnnotator)
import           WikiEL.WikiEntityClass                       (fromFiles,getNEClass)
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,parseStanfordNE,namedEntityAnnotator,resolvedUID)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),UIDCite(..),entityLinking,entityLinkings,buildEntityMentions,entityUID)
import           WikiEL.ETL.LoadData
-- For testing:
import           WikiEL.Misc                                  (IRange(..),untilOverlapOrNo,untilNoOverlap,relativePos, isContain,subVector)
import qualified NLP.Type.NamedEntity          as N
import qualified WikiEL.WikiEntityClass        as WC
import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.WordNet
import           WikiEL.Type.Equity
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser
import           WikiEL.WordNet
import qualified WikiEL.EntityLinking          as EL
import qualified WikiEL.Type.FileFormat        as F
import qualified WikiEL                        as WEL


--
import           OntoNotes.App.Load
import           OntoNotes.App.Util
import           OntoNotes.App.WikiEL
import           OntoNotes.Corpus.Load
import           OntoNotes.Format
import           OntoNotes.Mapping.FrameNet
import           OntoNotes.Type.ArgTable
import           OntoNotes.Type.SenseInventory

getFormatTimex :: (SentItem,[TagPos (Maybe Utf8)]) -> [Text]
getFormatTimex (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]

showFormatTimex :: (SentItem,[TagPos (Maybe Utf8)]) -> IO ()
showFormatTimex (s,a) = T.IO.putStrLn (T.intercalate "\n" (getFormatTimex (s,a)))

getFormatTimex' :: (SentItem,[TagPos (Maybe Text)]) -> [Text]
getFormatTimex' (s,a) = (underlineText (const "") (s^._2) (s^._3) a) ++ ["----------"] ++ [T.pack (show a)]

showFormatTimex' :: (SentItem,[TagPos (Maybe Text)]) -> IO ()
showFormatTimex' (s,a) = T.IO.putStrLn (T.intercalate "\n" (getFormatTimex' (s,a)))

formatLemmaPOS :: Token -> String
formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))

formatNER' :: [[Maybe Token]] -> [SentItem] -> [UIDCite EntityMentionUID (EL.EMInfo Text)] -> Box
formatNER' psents sentitems linked_mentions_resolved =
  let toks = concatMap (map snd . sentToTokens') psents
      tags = mapMaybe (linkedMentionToTagPOS toks) linked_mentions_resolved
      sents_tagged = map (addTag tags) sentitems
      doc1 = formatTaggedSentences sents_tagged
      doc2 = vcat top . intersperse (text "") . map (text.formatLinkedMention) $ linked_mentions_resolved
  in hsep 10 left [doc1,doc2]


runParser :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
          -> ([(Text,N.NamedEntityClass)] -> [EntityMention Text])
          -> Text
          -> IO ( [S.Sentence]
                , [Maybe SentenceIndex]
                , [(SentIdx,BeginEnd,Text)]                  
                , [[Token]]
                , [Maybe PennTree]
                , [Dependency]
                , Maybe [(SentItem, [TagPos (Maybe Utf8)])]
                , [UIDCite EntityMentionUID (EL.EMInfo Text)]
                )
runParser pp emTagger txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  lbstr_sutime <- BL.fromStrict <$> serializeTimex ann
  let psents = toListOf (D.sentence . traverse) pdoc
      sentidxs = getSentenceOffsets psents
      sentitems = map (addText txt) sentidxs
  
  mtmx <- case fmap fst (messageGet lbstr_sutime) :: Either String T.ListTimex of
    Left _ -> return Nothing
    Right rsutime -> do
      let sentswithtmx = addSUTime sentitems rsutime
      return (Just sentswithtmx)
  let parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken) tktokss

      unNER (NERSentence tokens) = tokens
      neTokens = concatMap (unNER . sentToNER) psents
      linked_mentions_all = emTagger neTokens
      linked_mentions_resolved
        = filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all
  return (psents,sents,sentitems,tokss,parsetrees,deps,mtmx,linked_mentions_resolved)


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


chooseFrame :: [(Text,Text,Int,Text,Text,Text,Text)] -> Maybe (Text,Text,Int,Text,Text,Text,Text)
chooseFrame [] = Nothing
chooseFrame xs = Just (maximumBy (compare `on` (^._3)) xs)


formatSense :: (Text,Text,Int,Text,Text,Text,Text) -> String
formatSense (sgrp,sn,num,txt_def,txt_frame,txt_fecore,txt_feperi) = 
  printf "%2s.%-6s (%4d cases) | %-40s | %-20s | %-40s      ------       %-30s " sgrp sn num txt_def txt_frame txt_fecore txt_feperi


formatSenses :: Bool  -- ^ doesShowOtherSense
             -> [((Text,Text), [(Text,Text)])]
             -> [((Text,Text),[(ArgPattern Text,Int)])]
             -> Text
             -> [(Text,Text,Int,Text,Text,Text,Text)]
             -> String
formatSenses doesShowOtherSense rolemap subcats lma lst 
  = let t = chooseFrame lst
    in "Top frame: "
       ++ printf " %-20s | %-40s      ------      %-30s\n"
            (fromMaybe "" (t^?_Just._5))
            (fromMaybe "" (t^?_Just._6))
            (fromMaybe "" (t^?_Just._7))
       ++ "--------------------------------------------------------------------------------------------------\n"            
       ++ maybe "" (formatRoleMap . (^._2))
            (do t1 <- t^?_Just._1
                t2 <- t^?_Just._2
                let sid = (lma, t1<>"."<>t2)
                find (\rm -> rm^._1 == sid) rolemap)
       ++ "\n--------------------------------------------------------------------------------------------------\n"       
       ++ if doesShowOtherSense
          then "\n\n\n*********************************************\n" ++ intercalate "\n" (map formatSense lst)
          else ""



formatNER psents sentitems linked_mentions_resolved =
  let toks = concatMap (map snd . sentToTokens) psents
      tags = mapMaybe (linkedMentionToTagPOS toks) linked_mentions_resolved
      sents_tagged = map (addTag tags) sentitems
      doc1 = formatTaggedSentences sents_tagged
      doc2 = vcat top . intersperse (text "") . map (text.formatLinkedMention) $ linked_mentions_resolved
  in hsep 10 left [doc1,doc2]


sentStructure :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
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
    Just sentswithtmx -> mapM_ showFormatTimex sentswithtmx
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

      forM_ (vps^..traverse.vp_lemma.to unLemma) $ \lma -> do
        putStrLn (printf "Verb: %-20s" lma)
        let senses = getSenses lma sensemap sensestat framedb ontomap
        (putStrLn . formatSenses False rolemap subcats lma) senses
        putStrLn "--------------------------------------------------------------------------------------------------"


sentStructure' :: HashMap Text Inventory
               -> HashMap (Text, Text) Int
               -> FrameDB
               -> HashMap Text [(Text, Text)]
               -> ([(Text,N.NamedEntityClass)] -> [EntityMention Text])
               -> ([Sentence], [Maybe SentenceIndex], [SentItem], [[Token]], [Maybe PennTree], [Dependency], Maybe [ (SentItem, [TagPos (Maybe Text)]) ] )
               -> IO ()
sentStructure' sensemap sensestat framedb ontomap emTagger loaded = do
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
        (putStrLn . formatSenses False) senses
        putStrLn "--------------------------------------------------------------------------------------------------"






-- -- abandoned code to simplify the resultant text.
      -- let tkns = zip [0..] (getTKTokens psent)
      -- tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)
      -- itr = mkAnnotatable (mkPennTreeIdx ptr)
      -- iltr = lemmatize lmap itr
      -- idltr = depLevelTree dep iltr
      -- vtree = verbTree vps idltr
      -- let lmaposs = concatMap (filter (\t -> isVerb (t^.token_pos))) $ tokss
      --     lmas = map (^.token_lemma) lmaposs

queryProcess :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> HashMap Text Inventory
             -> HashMap (Text,Text) Int
             -> FrameDB
             -> HashMap Text [(Text,Text)]
             -> ([(Text,N.NamedEntityClass)] -> [EntityMention Text])
             -> [((Text,Text), [(Text,Text)])]
             -> [((Text,Text),[(ArgPattern Text,Int)])]
             -> IO ()
queryProcess pp sensemap sensestat framedb ontomap emTagger rolemap subcats =
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
        (command,rest) = T.splitAt 3 input
    case command of
      ":l " -> do let fp = T.unpack (T.strip rest)
                  txt <- T.IO.readFile fp
                  sentStructure pp sensemap sensestat framedb ontomap emTagger rolemap subcats txt
      ":v " ->    sentStructure pp sensemap sensestat framedb ontomap emTagger rolemap subcats rest
      _     ->    putStrLn "cannot understand the command"
    putStrLn "=================================================================================================\n\n\n\n"



getSentStructure :: J ('Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
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
                Just sentswithtmx -> concat $ map getFormatTimex sentswithtmx

  let line3 = [ "-- WikiNamedEntityTagger ------------------------------------------------------------------------"
              , T.pack (render (formatNER psents sentitems linked_mentions_resolved))
              , "--------------------------------------------------------------------------------------------------"
              , "-- Sentence analysis -----------------------------------------------------------------------------"
              , "--------------------------------------------------------------------------------------------------" ]
              
  mlines <- flip mapM (zip5 ([0..] :: [Int]) psents sents mptrs deps) $ \(i,psent,_sent,mptr,_dep) -> do
    flip mapM mptr $ \ptr -> do
      let lemmamap = mkLemmaMap psent
          vps = verbPropertyFromPennTree lemmamap ptr
          clausetr = clauseStructure vps (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx ptr))

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


  
runAnalysis :: IO ()
runAnalysis = do
  -- let cfg = cfgG -- for the time being
  subcats <- loadVerbSubcat
  rolemap <- loadRoleMap
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  emTagger <- loadEMtagger reprFile [(WC.orgClass, orgItemFile), (WC.personClass, personItemFile), (WC.brandClass, brandItemFile)]  
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True)
                  )
    queryProcess pp sensemap sensestat framedb ontomap emTagger rolemap subcats


loadConfig = do
  let cfg = cfgG
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  emTagger <- loadEMtagger reprFile [(WC.orgClass, orgItemFile), (WC.personClass, personItemFile), (WC.brandClass, brandItemFile)]  
  return (sensemap,sensestat,framedb,ontomap,emTagger)


getAnalysis input config pp = do
  subcats <- loadVerbSubcat
  rolemap <- loadRoleMap  
  let (sensemap,sensestat,framedb,ontomap,emTagger) = config
  getSentStructure pp sensemap sensestat framedb ontomap emTagger rolemap subcats  input
    

--
--
-- wiki-ner test
--
--

{- 
main1 :: IO ()
main1 = do
  txt <- T.IO.readFile newsFileTxt

  emTagger <- loadEMtagger reprFile [(WC.orgClass, orgItemFile), (WC.personClass, personItemFile), (WC.brandClass, brandItemFile)]

  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (ner .~ True)
                  )
    let doc = Document txt (fromGregorian 2017 4 17)
    ann <- annotate pp doc
    rdoc <- protobufDoc ann
    case rdoc of
      Left _ -> return ()
      Right d -> do
        let psents = d ^.. D.sentence . traverse
            sentidxs = getSentenceOffsets psents
            sents = map (addText txt) sentidxs 
            unNER (NERSentence tokens) = tokens
            neTokens = concatMap (unNER . sentToNER) psents
            linked_mentions_all = emTagger neTokens
            linked_mentions_resolved
              = filter (\x -> let (_,_,pne) = _info x in case pne of Resolved _ -> True ; _ -> False) linked_mentions_all
            toks = concatMap (map snd . sentToTokens) psents
            tags = mapMaybe (linkedMentionToTagPOS toks) linked_mentions_resolved
            sents_tagged = map (addTag tags) sents
            doc1 = formatTaggedSentences sents_tagged
            doc2 = vcat top . intersperse (text "") . map (text.formatLinkedMention) $ linked_mentions_resolved
            doc = hsep 10 left [doc1,doc2]
        putStrLn (render doc)
-}

loadJVM = do
  pp <- prepare (def & (tokenizer .~ True)
                     . (words2sentences .~ True)
                     . (postagger .~ True)
                     . (lemma .~ True)
                     . (sutime .~ True)
                     . (constituency .~ True)
                     . (ner .~ True)
                )
  return pp

cfgG :: Config              
cfgG = Config { _cfg_sense_inventory_file  = "/data/groups/uphere/data/NLP/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
              , _cfg_semlink_file          = "/data/groups/uphere/data/NLP/SemLink/1.2.2c/vn-fn/VNC-FNF.s"
              , _cfg_statistics            = "/data/groups/uphere/data/NLP/run/20170717/OntoNotes_propbank_statistics_only_wall_street_journal_verbonly.txt"
              , _cfg_wsj_directory         = "/data/groups/uphere/data/NLP/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
              , _cfg_framenet_lubin        = "/data/groups/uphere/data/NLP/run/FrameNet_ListOfLexUnit.bin"
              , _cfg_framenet_framedir     = "/data/groups/uphere/data/NLP/FrameNet/1.7/fndata/fndata-1.7/frame" 
              , _cfg_wordnet_dict          = "/data/groups/uphere/data/NLP/dict"
              , _cfg_propbank_framedir     = "/data/groups/uphere/data/NLP/frames"
              , _cfg_wsj_corenlp_directory = "/data/groups/uphere/data/NLP/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
              }
