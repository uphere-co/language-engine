{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module SRL.Analyze where

import           Control.Error.Safe
import           Control.Lens                 ((^.),(^..),(.~),(&),_Just,to,_1,_2,_3)
import           Control.Monad                (forM_,join,void,when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Loops          (whileJust_)
import qualified Data.ByteString.Char8  as B
import           Data.Default                 (def)
import           Data.Function                (on)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.IntMap            as IM
import           Data.List                    (foldl',intercalate,maximumBy,sortBy)
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T.IO
import qualified Data.Vector            as V
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
import           NLP.Syntax.Format            (formatX'Tree)
import           NLP.Type.CoreNLP             (Sentence)
import           NLP.Type.NamedEntity         (NamedEntityClass(..))
import           NLP.Type.SyntaxProperty      (Voice)
import           OntoNotes.Corpus.Load        (senseInstStatistics)
import           OntoNotes.Type.SenseInventory (Inventory,inventory_lemma)
import           Text.Format.Dot              (mkLabelText)
import           WikiEL                       (extractFilteredEntityMentions)
import           WikiEL.EntityLinking         (EntityMention)
import           WikiEL.Misc                  (beg,end)
import           WikiEL.Run                   (runEL,loadWikiData,classFilesG,reprFileG)
import           WikiEL.WikiEntityClass       (brandClass,orgClass,personClass,locationClass,occupationClass,humanRuleClass,buildingClass)
import qualified WikiEL.WikiEntityClass             as WEC
import qualified WikiEL.WikiEntityTagger            as WET
--
import           SRL.Analyze.ARB               (mkARB)
import qualified SRL.Analyze.Config as Analyze
import           SRL.Analyze.CoreNLP           (runParser)
import           SRL.Analyze.Format            (dotMeaningGraph,formatDocStructure,showMatchedFrame)
import           SRL.Analyze.Match             (mkPAWSTriples,meaningGraph,tagMG)
import           SRL.Analyze.SentenceStructure (docStructure,mkWikiList)
import           SRL.Analyze.Type
-- import           SRL.Analyze.WikiEL            (brandItemFile,buildingItemFile,humanRuleItemFile,locationItemFile
--                                                ,occupationItemFile,orgItemFile,personItemFile,reprFile)
-- import           SRL.Analyze.WikiEL            (mkWikiList)

import           SRL.Statistics (getGraphFromMG)
--
import Debug.Trace
import qualified Data.Vector as V
import qualified WikiEL.EntityLinking   as EL
import qualified WikiEL.ETL.LoadData    as LD
import qualified WikiEL.Type.FileFormat as FF
import qualified WikiEL.Type.Wikidata   as WD
-- import qualified WikiEL.WikiEntityClass       as WEC
import qualified WikiEL.WikiNamedEntityTagger as WNET


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
                  mapM_ (T.IO.putStrLn . formatX'Tree) (dstr ^.. ds_sentStructures . traverse . _Just . ss_cpstr . traverse)
                  when (config^.Analyze.showDetail) $ do
                    print (dstr^.ds_mergedtags)
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


  -- entityResolve = WEL.disambiguateMentions .. seems to have a problem

newNETagger = do
  reprs <- LD.loadEntityReprs reprFileG
  let wikiTable = WET.buildEntityTable reprs
      wikiMap = foldl' f IM.empty reprs
        where f !acc (FF.EntityReprRow (WD.ItemID i) (WD.ItemRepr t)) = IM.insertWith (++) i [t] acc
  uidNEtags <- WEC.loadFiles classFilesG -- uidTagFiles
  let tagger = extractFilteredEntityMentions wikiTable uidNEtags
      disambiguatorWorker x (ys,t) =
        let lst = sortBy (flip compare `on` (length.snd)) .  mapMaybe (\y-> (y,) <$> IM.lookup (WD._itemID y) wikiMap) $ ys
       in case lst of
            [] -> x
            (r:_) -> let (i1,i2,_) = EL._info x
                         u = WEC.guessItemClass2 uidNEtags t
                         resolved = r^._1
                     in x { EL._info = (i1,i2,WNET.Resolved (resolved,u resolved)) }

  let disambiguator x =
        case ((^._3) . EL._info) x of
          WNET.AmbiguousUID (ys,t) -> disambiguatorWorker x (ys,t)
          WNET.UnresolvedUID t ->
            if t == Org || t == Person
            then
              let name0 = EL.entityName (EL._info x)
                  name = (T.replace "," "" . T.replace "." "") name0   -- try once more
                  tags' = WET.wikiAnnotator wikiTable (T.words name)
                  tags'' = filter (\(r,_)->end r-beg r>1) tags'
              in case tags'' of
                   [] -> x
                   _  -> let rids = (V.toList . snd . maximumBy (flip compare `on` (\(r,_) -> end r - beg r))) tags''
                         in disambiguatorWorker x (rids,t)
            else x
          _ -> x
  return (runEL tagger (map disambiguator))

loadConfig :: Bool
           -> LexDataConfig
           -> IO (AnalyzePredata,[Sentence]->[EntityMention Text])
loadConfig bypass_ner cfg = do
  apredata@(AnalyzePredata sensemap sensestat framedb ontomap rolemap subcats) <- loadAnalyzePredata cfg
  netagger <- if bypass_ner
                then return (const [])
                else newNETagger
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
