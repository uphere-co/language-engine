{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze where

import           Control.Lens                   ((^.),(.~),(&))
import           Control.Monad                  (forM_,void,when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Loops            (whileJust_)
import qualified Data.ByteString.Char8  as B
import           Data.Default                   (def)
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T.IO
import qualified Language.Java          as J
import           MWE.Util                       (mkTextFromToken)
import           System.Console.Haskeline       (runInputT,defaultSettings,getInputLine)
import           System.Environment             (getEnv)
import           System.Process                 (readProcess)
--
import           CoreNLP.Simple                 (prepare)
import           CoreNLP.Simple.Type            (tokenizer,words2sentences,postagger,lemma,sutime,constituency,ner)
import           FrameNet.Query.Frame           (loadFrameData)
import           HUKB.PPR                       (createUKBDB)
import           Lexicon.Mapping.OntoNotesFrameNet (mapFromONtoFN)
import           Lexicon.Query                  (adjustRolePattInsts,loadRoleInsts,loadRolePattInsts)
import           Lexicon.Type                   (RoleInstance)
import           Lexicon.Data                   (LexDataConfig(..),cfg_framenet_framedir
                                                ,cfg_rolemap_file
                                                ,cfg_sense_inventory_file
                                                ,cfg_verb_subcat_file
                                                ,cfg_wsj_directory
                                                ,cfg_wordnet_dict
                                                ,cfg_ukb_dictfile
                                                ,cfg_ukb_binfile
                                                ,loadSenseInventory
                                                )
import           NLP.Type.CoreNLP               (Sentence)
import           OntoNotes.Corpus.Load          (senseInstStatistics)
import           OntoNotes.Type.SenseInventory  (inventory_lemma)
import           Text.Format.Dot                (mkLabelText)
import           WikiEL.Type                    (EntityMention)
import           WikiEL.WikiNewNET              (newNETagger)
import           WordNet.Query                  (loadDB)
--
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP            (runParser)
import           SRL.Analyze.Format             (dotMeaningGraph,formatDocStructure,showMatchedFrame)
import           SRL.Analyze.Match.Frame        (mkTriples)
import           SRL.Analyze.Match.MeaningGraph (meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure  (docStructure,mkWikiList)
import           SRL.Analyze.Type
--

--
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
        frmdb = apredata^.analyze_framedb
    case command of
      ":l " -> do let fp = T.unpack (T.strip rest)
                  txt <- T.IO.readFile fp
                  dainput <- runParser pp txt
                  -- runUKB (apredata^.analyze_wordnet) (dainput^.dainput_sents,dainput^.dainput_mptrs)
                  dstr <- docStructure apredata netagger dainput
                  when (config^.Analyze.showDetail) $
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)
                  mapM_ (uncurry (showMatchedFrame frmdb)) . concatMap  (\s -> [(s^.ss_tagged,x) | x <- snd (mkTriples s)]) . catMaybes $ (dstr^.ds_sentStructures)

      ":v " -> do dainput <- runParser pp rest
                  -- runUKB (apredata^.analyze_wordnet) (dainput^.dainput_sents,dainput^.dainput_mptrs)
                  dstr <- docStructure apredata netagger dainput
                  when (config^.Analyze.showDetail) $ do
                    -- print (dstr^.ds_mergedtags)
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)

                  mapM_ (uncurry (showMatchedFrame frmdb)) . concatMap  (\s -> [(s^.ss_tagged,x) | x <- snd (mkTriples s)]) . catMaybes $ (dstr^.ds_sentStructures)
                  --
                  printMeaningGraph apredata (apredata^.analyze_rolemap) dstr
      _     ->    putStrLn "cannot understand the command"
    putStrLn "=================================================================================================\n\n\n\n"




printMeaningGraph :: AnalyzePredata -> [RoleInstance] -> DocStructure -> IO ()
printMeaningGraph apredata _rolemap dstr = do
  putStrLn "-------------"
  putStrLn "meaning graph"
  putStrLn "-------------"
  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      -- wikilst = mkWikiList dstr
  -- print (sstrs1 ^.. traverse . ss_ptr)
  -- print (sstrs1 ^.. traverse . ss_clausetr)

  let mgs = map (\sstr -> (sstr,meaningGraph apredata sstr)) sstrs1
  forM_ (zip mtokss (zip ([1..] :: [Int]) mgs)) $ \(mtks,(i,(sstr,mg'))) -> do
    let title = mkTextFromToken mtks
        wikilst = mkWikiList sstr -- sstrs1
        mg = tagMG mg' wikilst
    -- mapM_ print (mg^.mg_vertices)
    -- mapM_ print (mg^.mg_edges)
    -- putStrLn "====================="
    -- print mg
    -- putStrLn "====================="
    -- let mg' = squashRelFrame mg
    -- putStrLn "====================="


    -- putStrLn "-----------------"
    -- putStrLn "meaning graph ARB"
    -- putStrLn "-----------------"

    -- print mg
    -- print (getGraphFromMG mg)
    -- mapM_ print (mkARB rolemap mg)

    putStrLn "-----------------"
    putStrLn "meaning graph dot"
    putStrLn "-----------------"
    let dotstr = dotMeaningGraph (mkLabelText title) mg
    T.IO.putStrLn dotstr
    T.IO.writeFile ("test" ++ (show i) ++ ".dot") dotstr
    void (readProcess "dot" ["-Tpng","test" ++ (show i) ++ ".dot","-otest" ++ (show i) ++ ".png"] "")

loadConfig :: Bool
           -> LexDataConfig
           -> IO (AnalyzePredata,[Sentence]->[EntityMention Text])
loadConfig bypass_ner cfg = do
  apredata <- loadAnalyzePredata cfg
  createUKBDB (cfg^.cfg_ukb_binfile,cfg^.cfg_ukb_dictfile)
  netagger <- if bypass_ner
                then return (const [])
                else newNETagger
  return (apredata,netagger)


loadAnalyzePredata :: LexDataConfig -> IO AnalyzePredata
loadAnalyzePredata cfg = do
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  wndb <- loadDB (cfg^.cfg_wordnet_dict)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  rolemap <- loadRoleInsts (cfg^.cfg_rolemap_file)
  subcats <- adjustRolePattInsts <$> loadRolePattInsts (cfg^.cfg_verb_subcat_file)
  return (AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats wndb)


{-
getAnalysis :: DocAnalysisInput
            -> (HashMap Text Inventory
               ,HashMap (Text,Text) Int
               ,FrameDB
               ,HashMap Text [(Text,FNFrame)]
               ,([Sentence] -> [EntityMention Text])
               ,[RoleInstance]
               ,[RolePattInstance Voice])
            -> DocStructure
getAnalysis input config =
  let (sensemap,sensestat,framedb,ontomap,netagger,rolemap,subcats) = config
      apredata = AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats
  in docStructure apredata netagger input
-}

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
