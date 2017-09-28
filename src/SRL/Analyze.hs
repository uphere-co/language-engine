{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module SRL.Analyze where

import           Control.Lens                 ((^.),(^..),(.~),(&),_Just,to)
import           Control.Monad                (forM_,join,void,when)
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
import           Lexicon.Query                (adjustRolePattInsts,loadRoleInsts,loadRolePattInsts)
import           Lexicon.Type                 (RoleInstance,RolePattInstance)
import           Lexicon.Data                 (LexDataConfig(..),cfg_framenet_framedir
                                              ,cfg_rolemap_file
                                              ,cfg_sense_inventory_file
                                              ,cfg_verb_subcat_file
                                              ,cfg_wsj_directory
                                              ,loadSenseInventory
                                              )
import           NLP.Syntax.Format            (formatCPHierarchy)
import           NLP.Type.CoreNLP             (Sentence)
import           NLP.Type.NamedEntity         (NamedEntityClass)
import           NLP.Type.SyntaxProperty      (Voice)
import           OntoNotes.Corpus.Load        (senseInstStatistics)
import           OntoNotes.Type.SenseInventory (Inventory,inventory_lemma)
import           Text.Format.Dot              (mkLabelText)
-- import           WikiEL                       (loadEMtagger)
import           WikiEL.EntityLinking         (EntityMention)
import           WikiEL.Run                   (runEL,loadWikiData)
import           WikiEL.WikiEntityClass       (brandClass,orgClass,personClass,locationClass,occupationClass,humanRuleClass,buildingClass)
--
import           SRL.Analyze.ARB               (mkARB)
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP           (runParser)
import           SRL.Analyze.Format            (dotMeaningGraph,formatDocStructure,showMatchedFrame)
import           SRL.Analyze.Match             (mkPAWSTriples,meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure (docStructure)
import           SRL.Analyze.Type
import           SRL.Analyze.WikiEL            (brandItemFile,buildingItemFile,humanRuleItemFile,locationItemFile
                                               ,occupationItemFile,orgItemFile,personItemFile,reprFile)
import           SRL.Analyze.WikiEL            (mkWikiList)

import SRL.Statistics (getGraphFromMG)

-- | main query loop
--
queryProcess :: Analyze.Config
             -> J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> AnalyzePredata
             -> ([Sentence] -> [EntityMention Text])
             -> IO ()
queryProcess config pp apredata netagger =
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
        (command,rest) = T.splitAt 3 input
    case command of
      ":l " -> do let fp = T.unpack (T.strip rest)
                  txt <- T.IO.readFile fp
                  dstr <- docStructure apredata netagger <$> runParser pp txt
                  when (config^.Analyze.showDetail) $
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)
                  mapM_ (uncurry showMatchedFrame) . concatMap  (\s -> [(s^.ss_tagged,x) | x <- snd (mkPAWSTriples s)]) . catMaybes $ (dstr^.ds_sentStructures)

      ":v " -> do dstr <- docStructure apredata netagger <$> runParser pp rest
                  mapM_ (T.IO.putStrLn . formatCPHierarchy) (dstr ^.. ds_sentStructures . traverse . _Just . ss_cpstr . traverse)
                  when (config^.Analyze.showDetail) $
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)

                  mapM_ (uncurry showMatchedFrame) . concatMap  (\s -> [(s^.ss_tagged,x) | x <- snd (mkPAWSTriples s)]) . catMaybes $ (dstr^.ds_sentStructures)
                  --
                  printMeaningGraph (apredata^.analyze_rolemap) dstr
      _     ->    putStrLn "cannot understand the command"
    putStrLn "=================================================================================================\n\n\n\n"




printMeaningGraph :: [RoleInstance] -> DocStructure -> IO ()
printMeaningGraph rolemap dstr = do
  putStrLn "-------------"
  putStrLn "meaning graph"
  putStrLn "-------------"
  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      wikilst = mkWikiList dstr
  -- print (sstrs1 ^.. traverse . ss_ptr)
  -- print (sstrs1 ^.. traverse . ss_clausetr)

  let mgs = map meaningGraph sstrs1
  forM_ (zip mtokss (zip ([1..] :: [Int]) mgs)) $ \(mtks,(i,mg')) -> do
    let title = mkTextFromToken mtks
        mg = tagMG mg' wikilst
    mapM_ print (mg^.mg_vertices)
    mapM_ print (mg^.mg_edges)
    putStrLn "-----------------"
    putStrLn "meaning graph ARB"
    putStrLn "-----------------"

    print mg
    print (getGraphFromMG mg)
    mapM_ print (mkARB rolemap mg)
    
    putStrLn "-----------------"
    putStrLn "meaning graph dot"
    putStrLn "-----------------"
    let dotstr = dotMeaningGraph (T.unpack $ mkLabelText title) mg
    putStrLn dotstr
    writeFile ("test" ++ (show i) ++ ".dot") dotstr
    void (readProcess "dot" ["-Tpng","test" ++ (show i) ++ ".dot","-otest" ++ (show i) ++ ".png"] "")


loadConfig :: Bool
           -> LexDataConfig
           -> IO (AnalyzePredata,[Sentence]->[EntityMention Text])
loadConfig bypass_ner cfg = do
  apredata@(AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats) <- loadAnalyzePredata cfg
  netagger <- if bypass_ner
                then return (const [])
                else uncurry runEL <$> loadWikiData
  return (apredata,netagger)


loadAnalyzePredata :: LexDataConfig -> IO AnalyzePredata
loadAnalyzePredata cfg = do
  -- let cfg = cfgG
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  rolemap <- loadRoleInsts (cfg^.cfg_rolemap_file)
  subcats <- adjustRolePattInsts <$> loadRolePattInsts (cfg^.cfg_verb_subcat_file)
  return (AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats)


getAnalysis :: DocAnalysisInput
            -> (HashMap Text Inventory
               ,HashMap (Text,Text) Int
               ,FrameDB
               ,HashMap Text [(Text,Text)]
               ,([Sentence] -> [EntityMention Text])
               ,[RoleInstance]
               ,[RolePattInstance Voice])
            -> DocStructure
getAnalysis input config =
  let (sensemap,sensestat,framedb,ontomap,netagger,rolemap,subcats) = config
      apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  in docStructure apredata netagger input


loadJVM :: IO (J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline"))
loadJVM = prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True))


-- | main program entry point
--
runAnalysis :: LexDataConfig -> Analyze.Config -> IO ()
runAnalysis cfg acfg = do
  (apredata,netagger) <- loadConfig (acfg^.Analyze.bypassNER) cfg
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
    queryProcess acfg pp apredata netagger
