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
import           OntoNotes.App.Analyze.CoreNLP           (runParser)
import           OntoNotes.App.Analyze.SentenceStructure (getSentStructure,sentStructure)
import           OntoNotes.App.Load           (Config(..),cfg,cfgG,cfg_framenet_framedir
                                              ,cfg_rolemap_file
                                              ,cfg_sense_inventory_file
                                              ,cfg_verb_subcat_file
                                              ,cfg_wsj_directory
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
                  loaded <- runParser pp emTagger txt
                  sentStructure sensemap sensestat framedb ontomap emTagger rolemap subcats loaded
      ":v " -> do loaded <- runParser pp emTagger rest
                  sentStructure sensemap sensestat framedb ontomap emTagger rolemap subcats loaded
      _     ->    putStrLn "cannot understand the command"
    putStrLn "=================================================================================================\n\n\n\n"





  
runAnalysis :: Config -> IO ()
runAnalysis cfg = do
  subcats <- loadVerbSubcat (cfg^.cfg_verb_subcat_file)
  rolemap <- loadRoleMap (cfg^.cfg_rolemap_file)
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
  rolemap <- loadRoleMap (cfg^.cfg_rolemap_file)
  subcats <- loadVerbSubcat (cfg^.cfg_verb_subcat_file)
  return (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats)


getAnalysis input config pp = do
  let (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) = config
  getSentStructure pp sensemap sensestat framedb ontomap emTagger rolemap subcats  input
    

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

