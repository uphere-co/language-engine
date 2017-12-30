{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze where

import           Control.Lens                   ((^.),(^..),(.~),(&),_Just,to)
import           Control.Monad                  (forM_,void,when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Loops            (whileJust_)
import qualified Data.ByteString.Char8  as B
import           Data.Default                   (def)
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict    as HM
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap            as IM
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T.IO
import           Data.Tree                      (Forest)
import qualified Language.Java          as J
import           System.Console.Haskeline       (runInputT,defaultSettings,getInputLine)
import           System.Environment             (getEnv)
import           System.Process                 (readProcess)
--
import           CoreNLP.Simple                 (prepare)
import           CoreNLP.Simple.Type            (tokenizer,words2sentences,postagger,lemma,sutime,constituency,ner)
import           FrameNet.Query.Frame           (loadFrameData)
import           HUKB.PPR                       (createUKBDB)
import           Lexicon.Mapping.OntoNotesFrameNet (mapFromONtoFN)
import           Lexicon.Query                  (adjustRolePattInsts,loadRoleInsts,loadRolePattInsts,loadIdioms)
import           Lexicon.Data                   (LexDataConfig(..),cfg_framenet_framedir
                                                ,cfg_rolemap_file
                                                ,cfg_idiom_file
                                                ,cfg_sense_inventory_file
                                                ,cfg_verb_subcat_file
                                                ,cfg_wsj_directory
                                                ,cfg_wordnet_dict
                                                ,cfg_ukb_dictfile
                                                ,cfg_ukb_binfile
                                                ,loadSenseInventory
                                                )
import           Lexicon.Type                   (SenseID)
import           MWE.Util                       (mkTextFromToken)
import           NER.Load                       (loadCompanies)
import           NER.Type                       (CompanyInfo(..),alias,companyId)
import           NLP.Syntax.Format              (formatX'Tree)
import           NLP.Syntax.Type.XBar           (SPhase(..))
import           NLP.Type.CoreNLP               (Sentence)
import           OntoNotes.Corpus.Load          (senseInstStatistics)
import           OntoNotes.Type.SenseInventory  (inventory_lemma)
import           Text.Format.Dot                (mkLabelText)
import           Text.Search.New.Generic.SearchTree (addTreeItem)
import           WikiEL.Type                    (EntityMention)
import           WikiEL.WikiNewNET              (newNETagger)
import           WordNet.Query                  (loadDB)
--
import           SRL.Analyze.ARB                (mkARB)
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP            (runParser)
import           SRL.Analyze.Format             (dotMeaningGraph,formatDocStructure,showMatchedFrame)
import           SRL.Analyze.Match.Frame        (mkTriples)
import           SRL.Analyze.Match.MeaningGraph (meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure  (docStructure,mkWikiList)
import           SRL.Analyze.Type
--
import Debug.Trace


--
-- | main query loop
--
queryProcess :: Analyze.Config
             -> J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> AnalyzePredata
             -> ([Sentence] -> [EntityMention Text])
             -> (Forest (Either Int Text),IntMap CompanyInfo)
             -> IO ()
queryProcess config pp apredata netagger (forest,companyMap) =
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
        (command,rest) = T.splitAt 3 input
        frmdb = apredata^.analyze_framedb
    case command of
      ":l " -> do let fp = T.unpack (T.strip rest)
                  txt <- T.IO.readFile fp
                  dainput <- runParser pp txt
                  dstr <- docStructure apredata netagger (forest,companyMap) dainput
                  when (config^.Analyze.showDetail) $
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)
                  mapM_ (uncurry (showMatchedFrame frmdb)) . concatMap (\s -> [(s^.ss_tagged,x)| x <- mkTriples s ]) . catMaybes $ (dstr^.ds_sentStructures)

      ":v " -> do dainput <- runParser pp rest
                  dstr <- docStructure apredata netagger (forest,companyMap) dainput
                  mapM_ (T.IO.putStrLn . formatX'Tree SPH1) (dstr ^.. ds_sentStructures . traverse . _Just . ss_x'trs . traverse)
                  when (config^.Analyze.showDetail) $ do
                    mapM_ T.IO.putStrLn (formatDocStructure (config^.Analyze.showFullDetail) dstr)

                  mapM_ (uncurry (showMatchedFrame frmdb)) . concatMap (\s -> [(s^.ss_tagged,x)|  x <- mkTriples s]) . catMaybes $ (dstr^.ds_sentStructures)
                  --
                  printMeaningGraph apredata companyMap dstr
                  --
      _     ->    putStrLn "cannot understand the command"
    putStrLn "=================================================================================================\n\n\n\n"




printMeaningGraph :: AnalyzePredata -> IntMap CompanyInfo -> DocStructure -> IO ()
printMeaningGraph apredata companyMap dstr = do
  putStrLn "-------------"
  putStrLn "meaning graph"
  putStrLn "-------------"
  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      wikilsts = map (mkWikiList companyMap) sstrs1

  let mgs = map (\sstr -> (sstr,meaningGraph apredata sstr)) sstrs1
  forM_ (zip mtokss (zip ([1..] :: [Int]) mgs)) $ \(mtks,(i,(sstr,mg'))) -> do
    let title = mkTextFromToken mtks
        wikilst = mkWikiList companyMap sstr -- sstrs1
        mg = tagMG mg' wikilst
        x'trs = sstr^.ss_x'trs

    putStrLn "-----------------"
    putStrLn "meaning graph dot"
    putStrLn "-----------------"
    let dotstr = dotMeaningGraph (Just (mkLabelText title)) mg
    T.IO.putStrLn dotstr
    T.IO.writeFile ("test" ++ (show i) ++ ".dot") dotstr
    void (readProcess "dot" ["-Tpng","test" ++ (show i) ++ ".dot","-otest" ++ (show i) ++ ".png"] "")
    --
    mapM_ print (mkARB (apredata^.analyze_rolemap) mg) 


loadConfig :: (Bool,Bool)
           -> LexDataConfig
           -> IO (AnalyzePredata,[Sentence]->[EntityMention Text],Forest (Either Int Text),IntMap CompanyInfo)
loadConfig (bypass_ner,bypass_textner) cfg = do
  apredata <- loadAnalyzePredata cfg
  createUKBDB (cfg^.cfg_ukb_binfile,cfg^.cfg_ukb_dictfile)
  netagger <- if bypass_ner
                then return (const [])
                else newNETagger
  (forest,companyMap) <-
    if bypass_textner
      then return ([],IM.empty)
      else do companies <- loadCompanies
              let companyMap = IM.fromList (map (\x -> (x^.companyId,x)) companies)
                  clist = do c <- companies
                             let cid = c^.companyId
                             a <- c^.alias
                             return (cid,T.words a)
              let forest = foldr addTreeItem [] clist  -- [(c^.companyId,c^.alias) |  c <- companies] -- Temporary. Tokenization should be done by CoreNLP.
              return (forest,companyMap)
  return (apredata,netagger,forest,companyMap)



--
-- | Load all pre-analysis data
--
loadAnalyzePredata :: LexDataConfig -> IO AnalyzePredata
loadAnalyzePredata cfg = do
  framedb <- loadFrameData (cfg^.cfg_framenet_framedir)
  wndb <- loadDB (cfg^.cfg_wordnet_dict)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  rolemap <- loadRoleInsts (cfg^.cfg_rolemap_file)
  idioms <- loadIdioms (cfg^.cfg_idiom_file)  -- return (HM.empty) --  for the time being, turn off idiom
  -- print idioms
  subcats <- adjustRolePattInsts <$> loadRolePattInsts (cfg^.cfg_verb_subcat_file)
  return (AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats wndb idioms)



loadJVM :: IO (J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline"))
loadJVM = prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (sutime .~ True)
                       . (constituency .~ True)
                       . (ner .~ True))


--
-- | main program entry point
--
runAnalysis :: LexDataConfig -> Analyze.Config -> IO ()
runAnalysis cfg acfg = do
  (apredata,netagger,forest,companyMap) <- loadConfig (acfg^.Analyze.bypassNER,acfg^.Analyze.bypassTEXTNER) cfg

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
    queryProcess acfg pp apredata netagger (forest,companyMap)
