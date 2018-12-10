{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module SRL.Analyze where

import           Control.Lens             ( (^.), (^..), (.~), (&), _Just )
import           Control.Monad            ( forM_, void, when )
import           Control.Monad.IO.Class   ( liftIO )
import           Control.Monad.Loops      ( whileJust_ )
import qualified Data.ByteString.Char8 as B
import           Data.Default             ( def )
import qualified Data.HashMap.Strict as HM
import           Data.IntMap              ( IntMap )
import qualified Data.IntMap as IM
import           Data.Maybe               ( catMaybes )
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Language.Java as J
import           System.Console.Haskeline ( runInputT, defaultSettings, getInputLine )
import           System.Environment       ( getEnv )
import           System.Process           ( readProcess )
------ other language-engine
import           CoreNLP.Simple           ( prepare )
import           CoreNLP.Simple.Type      ( tokenizer, words2sentences
                                          , postagger, lemma, sutime
                                          , constituency, ner
                                          )
import           FrameNet.Query.Frame     ( FrameDB, loadFrameData )
import           HUKB.PPR                 ( createUKBDB )
import           Lexicon.Mapping.OntoNotesFrameNet ( mapFromONtoFN )
import           Lexicon.Query            ( adjustRolePattInsts, loadRoleInsts
                                          , loadRolePattInsts, loadIdioms
                                          )
import           Lexicon.Data             ( loadSenseInventory )
import           MWE.Util                 ( mkTextFromToken )
import           NER.Load                 ( getCompanyListFromJSON )
import           NER.Type                 ( CompanyInfo(..), alias, companyId )
import           NLP.Syntax.Format        ( formatX'Tree )
import           NLP.Syntax.Type.XBar     ( SPhase(..) )
import           OntoNotes.Corpus.Load    ( senseInstStatistics )
import           OntoNotes.Type.SenseInventory ( inventory_lemma )
import           Text.Format.Dot          ( mkLabelText )
import           Text.Search.New.Generic.SearchTree ( addTreeItem )
import           WikiEL.NETagger          ( newNETagger )
import           WikiEL.Type              ( emptyNETagger )
import           WordNet.Query            ( loadDB )
--
import           SRL.Analyze.MeaningTree  ( mkMeaningTree )
import           SRL.Analyze.Config       ( SRLConfig
                                          , srlcfg_framenet_framedir
                                          , srlcfg_rolemap_file
                                          , srlcfg_idiom_file
                                          , srlcfg_sense_inventory_file
                                          , srlcfg_verb_subcat_file
                                          , srlcfg_wsj_directory
                                          , srlcfg_wordnet_dict
                                          , srlcfg_ukb_dictfile
                                          , srlcfg_ukb_binfile
                                          , srlcfg_company_file
                                          , srlcfg_wiki_dir
                                          )
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP      ( runParser )
import           SRL.Analyze.Format       ( dotMeaningGraph
                                          , formatDocStructure
                                          , showMatchedFrame
                                          )
import           SRL.Analyze.Format.OGDF  ( mkOGDFSVG )
import           SRL.Analyze.Match.Frame  ( mkTriples )
import           SRL.Analyze.Match.MeaningGraph ( meaningGraph, tagMG )
import           SRL.Analyze.SentenceStructure ( docStructure, mkWikiList )
import           SRL.Analyze.Type         ( AnalysisData(..)
                                          , CompanyMap(..)
                                          , ConsoleOutput(..)
                                          , DocStructure
                                          , SRLData(SRLData)
                                          , analysis_CompanyMap
                                          , analysis_SRLData
                                          , cmap_map
                                          , emptyCompanyMap
                                          , srldata_framedb
                                          , srldata_rolemap
                                          , ds_mtokenss
                                          , ds_sentStructures
                                          , ss_tagged
                                          , ss_x'trs
                                          , outputX'tree
                                          , outputDocStructure
                                          , outputMatchedFrames
                                          )


consoleOutput :: FrameDB -> DocStructure -> ConsoleOutput
consoleOutput frmdb dstr =
  let sstrs = catMaybes (dstr^.ds_sentStructures)
      matchedframes = do s <- sstrs
                         x <- mkTriples s
                         pure (s^.ss_tagged,x)
  in ConsoleOutput { _outputX'tree = T.unlines (map (formatX'Tree SPH1) (dstr ^.. ds_sentStructures . traverse . _Just . ss_x'trs . traverse))
                   , _outputDocStructure = T.unlines (formatDocStructure True dstr)
                   , _outputMatchedFrames = T.unlines (concatMap (uncurry (showMatchedFrame frmdb)) matchedframes)
                   }


-- | main query loop
--
queryProcess :: Analyze.Config
             -> J.J ('J.Class "edu.stanford.nlp.pipeline.AnnotationPipeline")
             -> AnalysisData
             -> IO ()
queryProcess config pp adata =
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
        (command,rest) = T.splitAt 3 input
        frmdb = adata^.analysis_SRLData.srldata_framedb
    case command of
      ":l " -> do let fp = T.unpack (T.strip rest)
                  txt <- T.IO.readFile fp
                  dainput <- runParser pp txt
                  dstr <- docStructure adata dainput
                  let cout = consoleOutput frmdb dstr
                  T.IO.putStrLn (cout^.outputX'tree)
                  when (config^.Analyze.showDetail) $
                    T.IO.putStrLn (cout^.outputDocStructure)
                  T.IO.putStrLn (cout^.outputMatchedFrames)
      ":v " -> do dainput <- runParser pp rest
                  dstr <- docStructure adata dainput
                  let cout = consoleOutput frmdb dstr
                  T.IO.putStrLn (cout^.outputX'tree)
                  when (config^.Analyze.showDetail) $
                    T.IO.putStrLn (cout^.outputDocStructure)
                  T.IO.putStrLn (cout^.outputMatchedFrames)
                  printMeaningGraph
                    (adata^.analysis_SRLData)
                    (adata^.analysis_CompanyMap.cmap_map)
                    dstr
      _     ->    putStrLn "cannot understand the command"
    putStrLn "=================================================================================================\n\n\n\n"


printMeaningGraph :: SRLData -> IntMap CompanyInfo -> DocStructure -> IO ()
printMeaningGraph sdata companyMap dstr = do
  putStrLn "-------------"
  putStrLn "meaning graph"
  putStrLn "-------------"
  let sstrs1 = catMaybes (dstr^.ds_sentStructures)
      mtokss = (dstr ^. ds_mtokenss)
      mgs = map (\sstr -> (sstr,meaningGraph sdata sstr)) sstrs1
  forM_ (zip mtokss (zip ([1..] :: [Int]) mgs)) $ \(mtks,(i,(sstr,mg'))) -> do
    let title = mkTextFromToken mtks
        wikilst = mkWikiList companyMap sstr
        mg = tagMG mg' wikilst

    putStrLn "-----------------"
    putStrLn "meaning graph dot"
    putStrLn "-----------------"
    let dotstr = dotMeaningGraph (Just (mkLabelText title)) mg
    T.IO.putStrLn dotstr
    putStrLn "-----------------"
    putStrLn "create dot png"
    putStrLn "-----------------"
    T.IO.writeFile ("test" ++ (show i) ++ ".dot") dotstr
    void (readProcess "dot" ["-Tpng","test" ++ (show i) ++ ".dot","-otest" ++ (show i) ++ ".png"] "")

    putStrLn "-----------------"
    putStrLn "creating meaning graph OGDF-SVG"
    putStrLn "-----------------"
    mkOGDFSVG ("testogdf" ++ show i ++ ".svg") mg
    --
    putStrLn "-----------------"
    putStrLn "meaning tree"
    putStrLn "-----------------"
    mapM_ print (mkMeaningTree (sdata^.srldata_rolemap) mg)


-- | Load SRLData
loadSRLData :: SRLConfig -> IO SRLData
loadSRLData cfg = do
  framedb <- loadFrameData (cfg^.srlcfg_framenet_framedir)
  wndb <- loadDB (cfg^.srlcfg_wordnet_dict)
  let ontomap = HM.fromList mapFromONtoFN
  sensestat <- senseInstStatistics (cfg^.srlcfg_wsj_directory)
  sis <- loadSenseInventory (cfg^.srlcfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  rolemap <- loadRoleInsts (cfg^.srlcfg_rolemap_file)
  idioms <- loadIdioms (cfg^.srlcfg_idiom_file)
  subcats <- adjustRolePattInsts <$> loadRolePattInsts (cfg^.srlcfg_verb_subcat_file)
  return (SRLData sensemap sensestat framedb ontomap rolemap subcats wndb idioms)


-- TODO: integrate bypass options into some config.
-- | Load AnalysisData = SRL Data + NER Data
loadConfig ::
     (Bool,Bool)
  -> SRLConfig
  -> IO AnalysisData
loadConfig (bypass_ner,bypass_textner) cfg = do
  sdata <- loadSRLData cfg
  createUKBDB (cfg^.srlcfg_ukb_binfile,cfg^.srlcfg_ukb_dictfile)
  netagger <- if bypass_ner
                then pure emptyNETagger
                else newNETagger (cfg^.srlcfg_wiki_dir)
  cmap <-
    if bypass_textner
      then pure emptyCompanyMap
      else do companies <- getCompanyListFromJSON (cfg^.srlcfg_company_file)
              let companyMap = IM.fromList (map (\x -> (x^.companyId,x)) companies)
                  clist = do c <- companies
                             let cid = c^.companyId
                             a <- c^.alias
                             pure (cid,T.words a)
              let forest = foldr addTreeItem [] clist
              pure (CompanyMap forest companyMap)
  pure $
    AnalysisData sdata netagger cmap


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
runAnalysis :: SRLConfig -> Analyze.Config -> IO ()
runAnalysis cfg acfg = do
  adata <-
    loadConfig (acfg^.Analyze.bypassNER,acfg^.Analyze.bypassTEXTNER) cfg
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
    queryProcess acfg pp adata
