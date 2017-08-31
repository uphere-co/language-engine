{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SRL.Analyze where

import           Control.Lens                 ((^.),(^..),(.~),(&),(%~),at)
import           Control.Monad                (forM_,void,when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Loops          (whileJust_)
import qualified Data.ByteString.Char8  as B
import           Data.Default                 (def)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T.IO
import qualified Language.Java          as J
import           MWE.Util                     (mkTextFromToken)
import           System.Console.Haskeline     (runInputT,defaultSettings,getInputLine)
import           System.Environment           (getEnv)
import           System.Process               (readProcess)
--
import           CoreNLP.Simple               (prepare)
import           CoreNLP.Simple.Type          (tokenizer,words2sentences,postagger,lemma,sutime,constituency,ner)
import           FrameNet.Query.Frame         (FrameDB,loadFrameData)
import           Lexicon.Mapping.OntoNotesFrameNet (mapFromONtoFN)
import           Lexicon.Query                (loadRoleInsts,loadRolePattInsts)
import           Lexicon.Type                 (ArgPattern(..),RoleInstance,RolePattInstance)
import           NLP.Syntax.Type              (Voice)
import           NLP.Type.NamedEntity         (NamedEntityClass)
import           Text.Format.Dot              (mkLabelText)
import           WikiEL                       (loadEMtagger)
import           WikiEL.EntityLinking         (EntityMention(..))
import           WikiEL.WikiEntityClass       (brandClass,orgClass,personClass,locationClass,occupationClass,humanRuleClass,buildingClass)
--
import           OntoNotes.App.Load           (Config(..),cfg,cfgG,cfg_framenet_framedir
                                              ,cfg_rolemap_file
                                              ,cfg_sense_inventory_file
                                              ,cfg_verb_subcat_file
                                              ,cfg_wsj_directory
                                              ,loadSenseInventory
                                              )
import           OntoNotes.Corpus.Load        (senseInstStatistics)
import           OntoNotes.Type.SenseInventory (Inventory,inventory_lemma)
--
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP           (runParser)
import           SRL.Analyze.Format            (dotMeaningGraph,formatDocStructure,showMatchedFrame)
import           SRL.Analyze.Match             (allPAWSTriplesFromDocStructure,meaningGraph)
import           SRL.Analyze.SentenceStructure (docStructure)
import           SRL.Analyze.Type
import           SRL.Analyze.WikiEL            (brandItemFile,buildingItemFile,humanRuleItemFile,locationItemFile
                                               ,occupationItemFile,orgItemFile,personItemFile,reprFile)


-- | main query loop
--
queryProcess :: Analyze.Config
             -> J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> AnalyzePredata
             -> ([(Text,NamedEntityClass)] -> [EntityMention Text])
             -> IO ()
queryProcess config pp apredata emTagger =
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
        (command,rest) = T.splitAt 3 input
    case command of
      ":l " -> do let fp = T.unpack (T.strip rest)
                  txt <- T.IO.readFile fp
                  dstr <- docStructure apredata emTagger <$> runParser pp txt
                  when (config^.Analyze.showDetail) $
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)
                  (mapM_ showMatchedFrame . concat . allPAWSTriplesFromDocStructure) dstr
      ":v " -> do dstr <- docStructure apredata emTagger <$> runParser pp rest
                  when (config^.Analyze.showDetail) $ 
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)
                  (mapM_ showMatchedFrame . concat . allPAWSTriplesFromDocStructure) dstr
                  --
                  putStrLn "-------------"
                  putStrLn "meaning graph"
                  putStrLn "-------------"
                  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
                      mtokss = (dstr ^. ds_mtokenss)

                  print (sstrs1 ^.. traverse . ss_ptr)
                  print (sstrs1 ^.. traverse . ss_clausetr)
                  
                  let mgs = map meaningGraph sstrs1
                  forM_ (zip mtokss (zip [1..] mgs)) $ \(mtks,(i,mg'')) -> do
                    title <- mkTextFromToken mtks
                    let isEntity x = case x of
                          MGEntity {..} -> True
                          otherwise     -> False
                    let twiki = [((0,0),"-NamedEntityA"),((1,1),"-NamedEntityB")]
                    let mg' = map (\x -> if (x ^. mv_range) `elem` (map (\(x,y) -> x) twiki) && isEntity x
                                        then (x & mv_text .~ (T.append (x ^. mv_text) (fromMaybe "" $ lookup (x ^. mv_range) twiki)))
                                        else id x) (mg'' ^. mg_vertices) -- (filter isEntity (mg ^. mg_vertices))
                        mg = MeaningGraph mg' (mg'' ^. mg_edges)
                    mapM_ print (mg^.mg_vertices)
                    mapM_ print (mg^.mg_edges)
                    putStrLn "-----------------"
                    putStrLn "meaning graph dot"
                    putStrLn "-----------------"
                    let dotstr = dotMeaningGraph (T.unpack $ mkLabelText title) mg
                    putStrLn dotstr
                    writeFile ("test" ++ (show i) ++ ".dot") dotstr
                    void (readProcess "dot" ["-Tpng","test" ++ (show i) ++ ".dot","-otest" ++ (show i) ++ ".png"] "")
      _     ->    putStrLn "cannot understand the command"
    putStrLn "=================================================================================================\n\n\n\n"


loadConfig
  :: IO (HashMap Text Inventory
        ,HashMap (Text, Text) Int
        ,FrameDB
        ,HashMap Text [(Text, Text)]
        ,[(Text, NamedEntityClass)] -> [EntityMention Text]
        ,[RoleInstance]
        ,[RolePattInstance Voice]
        )
loadConfig = do
  (AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats) <- loadAnalyzePredata
  emTagger <- loadEMtagger reprFile [ (orgClass, orgItemFile), (personClass, personItemFile), (brandClass, brandItemFile)
                                    , (locationClass, locationItemFile), (occupationClass, occupationItemFile)
                                    , (humanRuleClass, humanRuleItemFile), (buildingClass, buildingItemFile) ]
  return (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats)

loadAnalyzePredata :: IO AnalyzePredata
loadAnalyzePredata = do
  let cfg = cfgG
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  rolemap <- loadRoleInsts (cfg^.cfg_rolemap_file)
  subcats <- loadRolePattInsts (cfg^.cfg_verb_subcat_file)
  return (AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats)

getAnalysis input config =
  let (sensemap,sensestat,framedb,ontomap,emTagger,rolemap,subcats) = config
      apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  in docStructure apredata emTagger input


 
loadJVM = prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True))


-- | main program entry point
--
runAnalysis :: Config -> Analyze.Config -> IO ()
runAnalysis cfg acfg = do
  subcats <- loadRolePattInsts (cfg^.cfg_verb_subcat_file)
  rolemap <- loadRoleInsts (cfg^.cfg_rolemap_file)
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  emTagger <- loadEMtagger reprFile [(orgClass, orgItemFile), (personClass, personItemFile), (brandClass, brandItemFile)]
  let apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
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
    queryProcess acfg pp apredata emTagger
