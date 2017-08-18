{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module OntoNotes.App.Analyze where

import           Control.Lens                 ((^.),(.~),(&))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Loops          (whileJust_)
import qualified Data.ByteString.Char8  as B
import           Data.Default                 (def)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.Text                    (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T.IO
import qualified Language.Java          as J
import           System.Console.Haskeline     (runInputT,defaultSettings,getInputLine)
import           System.Environment           (getEnv)
--
import           CoreNLP.Simple               (prepare)
import           CoreNLP.Simple.Type          (tokenizer,words2sentences,postagger,lemma,sutime,constituency,ner)
import           FrameNet.Query.Frame         (FrameDB,loadFrameData)
import           NLP.Type.NamedEntity         (NamedEntityClass)
import           WikiEL                       (loadEMtagger)
import           WikiEL.EntityLinking         (EntityMention(..))
import           WikiEL.WikiEntityClass       (brandClass,orgClass,personClass)
--
import           OntoNotes.App.Analyze.SentenceStructure (getSentStructure,sentStructure)
import           OntoNotes.App.Load           (Config(..),cfg,cfg_framenet_framedir
                                              ,cfg_sense_inventory_file,cfg_wsj_directory
                                              ,loadSenseInventory,loadRoleMap,loadVerbSubcat
                                              )
import           OntoNotes.App.WikiEL         (brandItemFile,orgItemFile,personItemFile,reprFile)
import           OntoNotes.Corpus.Load        (senseInstStatistics)
import           OntoNotes.Mapping.FrameNet   (mapFromONtoFN)
import           OntoNotes.Type.ArgTable      (ArgPattern(..))
import           OntoNotes.Type.SenseInventory (Inventory,inventory_lemma)




queryProcess :: J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> HashMap Text Inventory
             -> HashMap (Text,Text) Int
             -> FrameDB
             -> HashMap Text [(Text,Text)]
             -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
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
  emTagger <- loadEMtagger reprFile [(orgClass, orgItemFile), (personClass, personItemFile), (brandClass, brandItemFile)]  
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
  emTagger <- loadEMtagger reprFile [(orgClass, orgItemFile), (personClass, personItemFile), (brandClass, brandItemFile)]
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
