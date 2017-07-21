{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens          hiding (Level)
import           Control.Monad.Loops
import           Control.Monad.IO.Class           (liftIO)
import           Data.Binary
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Foldable
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List                        (foldl',minimumBy,sort,sortBy,zip4)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified Data.Text.Lazy.IO          as T.L.IO
import           Language.Java              as J
import           System.Console.Haskeline
import           System.Directory
import           System.Directory.Tree
import           System.Environment
import           System.FilePath
import           System.IO
import           Text.PrettyPrint.Boxes    hiding ((<>))
import           Text.Printf
import           Text.Taggy.Lens
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Common (fr_frame)
import           FrameNet.Type.LexUnit
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           SRL.Format                                  (formatBitree,showVerb)
import           SRL.Feature
import           SRL.Feature.Clause
import           SRL.Feature.Dependency
import           SRL.Feature.Verb
--
import           OntoNotes.Parser.Sense
import           OntoNotes.Parser.SenseInventory




data VorN = V | N deriving Eq


data Config = Config { _cfg_sense_inventory_file :: FilePath
                     -- , _cfg_semlink_file         :: FilePath
                     , _cfg_statistics           :: FilePath
                     , _cfg_wsj_directory        :: FilePath
                     , _cfg_framenet_file        :: FilePath
                     }

makeLenses ''Config
              
cfg = Config { _cfg_sense_inventory_file = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
             -- , _cfg_semlink_file = "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VNC-FNF.s"
             , _cfg_statistics = "run/OntoNotes_propbank_statistics_only_wall_street_journal_verbonly.txt"
             , _cfg_wsj_directory = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
             , _cfg_framenet_file = "/home/wavewave/repo/srcp/HFrameNet/run/FrameNet_ListOfLexUnit.bin"
             }
  


loadSenseInventory dir = do
  cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts)
  flip traverse fs $ \f -> do
    let fp = dir  </> f
    txt <- T.L.IO.readFile fp
    case txt ^? html . allNamed (only "inventory") of
      Nothing -> error "nothing"
      Just f -> case p_inventory f of
                  Left err -> error err
                  Right c  -> return c

{- 
createVNFNDB :: VNFNMappingData -> HashMap (Text,Text) [Text]
createVNFNDB semlink = 
  let lst = map (\c-> ((c^.vnc_vnmember,c^.vnc_class),c^.vnc_fnframe)) (semlink^.vnfnmap_vnclslst)
  in foldl' (\(!acc) (k,v) -> HM.insertWith (++) k [v] acc) HM.empty lst


loadSemLink file = do
  txt <- T.L.IO.readFile file
  case txt ^? html . allNamed (only "verbnet-framenet_MappingData") of
    Nothing -> error "nothing"
    Just f -> case p_vnfnmappingdata f of
                Left err -> error err
                Right c -> return c
-}

loadStatistics file = do
  txt <- T.IO.readFile file
  return $ map ((\(l:_:f:_) -> (l,f)) . T.words) (T.lines txt)


loadFrameNet file = do
  bstr <- BL.readFile file
  let lst = decode bstr :: [LexUnit]
      lexunitdb = foldl' insertLU emptyDB lst
  return lexunitdb
  


verbnet semlinkmap lma txt =
  let (_,cls') = T.breakOn "-" txt
  in if T.null cls'
     then text (printf "%-43s" ("" :: String))
     else let cls = T.tail cls'
              frms = fromMaybe [] (HM.lookup (lma,cls) semlinkmap)
          in text (printf "%-8s -> " cls) <+>
             vcat top (if null frms
                       then [text (printf "%-30s" ("":: String))]
                       else map (text.printf "%-30s") frms
                      )

framesFromLU :: LexUnitDB -> Text -> [Text]
framesFromLU ludb lma = do
  i <- fromMaybe [] (HM.lookup lma (ludb^.nameIDmap))
  l <- maybeToList (IM.lookup i (ludb^.lexunitDB))
  frm <- maybeToList (l^.lexunit_frameReference^.fr_frame)
  return frm
  

senseInstStatistics :: FilePath -> IO (HashMap (Text,Text) Int)
senseInstStatistics basedir = do
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      sfiles = filter (\x -> takeExtensions x == ".sense") fps

  sinstss <- flip mapM sfiles $ \fp -> do
    txt <- T.IO.readFile fp
    -- print fp
    let lst = T.lines txt
        wss = map T.words lst
    case traverse parseSenseInst wss of
      Left err -> error err
      Right lst -> return lst

  let sinsts = concat sinstss
      sinsts_verb = filter (\s-> T.last (s^.sinst_sense) == 'v') sinsts  
      ks = map (\s -> ( T.init (T.init (s^.sinst_sense)) ,s^.sinst_sense_num)) sinsts_verb
      acc = foldl' (\(!acc) k -> HM.insertWith (+) k 1 acc) HM.empty ks
  -- mapM_ (putStrLn.formatStat) . sortBy (flip compare `on` snd) . HM.toList $ acc
  return acc








convertToken_charIndex :: TK.Token -> Maybe Token
convertToken_charIndex t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8 <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8 <$> (t^.TK.pos)
  l <- cutf8 <$> (t^.TK.lemma)
  return (Token (b,e) w p l)

formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))


runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
  
      parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken_charIndex) tktokss
  return (psents,sents,tokss,parsetrees,deps)


formatSenses lma sensemap sensestat = do
  let lmav = lma <> "-v"
  si <- maybeToList (HM.lookup lmav sensemap)
  s <- si^.inventory_senses
  let num = fromMaybe 0 (HM.lookup (lma,s^.sense_n) sensestat)
      txt_def = T.take 40 (s^.sense_name)
      txt1 = printf "%2s.%-6s (%4d cases) | %-40s | " (s^.sense_group) (s^.sense_n) num txt_def
  return txt1 


sentStructure pp sensemap sensestat txt = do
  (psents,sents,tokss,mptrs,deps) <- runParser pp txt
  flip mapM_ (zip4 psents sents mptrs deps) $ \(psent,sent,mptr,dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let tkns = zip [0..] (getTKTokens psent)
          tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)
      
      let itr = mkAnnotatable (mkPennTreeIdx ptr)
          lmap= mkLemmaMap psent
          iltr = lemmatize lmap itr
          idltr = depLevelTree dep iltr
          vps = verbPropertyFromPennTree lmap ptr
          vtree = verbTree vps idltr
      putStrLn "--------------------------------------------------------------------------------------------------"
      T.IO.putStrLn  . T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip [0..] . map snd . toList $ ptr

      putStrLn "--------------------------------------------------------------------------------------------------"
      let lmaposs = concatMap (filter (\t -> isVerb (t^.token_pos))) $ tokss
          lmas = map (^.token_lemma) lmaposs
      mapM_ (putStrLn . formatLemmaPOS) lmaposs 
      -- mapM_ (T.IO.putStrLn . formatBitree (^._2.to (showVerb tkmap))) vtree
      -- putStrLn "---------------------------------------------------------------"
      showClauseStructure lmap ptr
      -- putStrLn "---------------------------------------------------------------"
      -- (T.IO.putStrLn . prettyPrint 0) ptr
      putStrLn "================================================================================================="
      
      forM_ lmas $ \lma -> do
        putStrLn (printf "Verb: %-20s" lma)
        mapM_ putStrLn (formatSenses lma sensemap sensestat)
        putStrLn "--------------------------------------------------------------------------------------------------"  
        






queryProcess pp sensemap sensestat = do
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
    sentStructure pp sensemap sensestat input
    

main = do
  ludb <- loadFrameNet (cfg^.cfg_framenet_file)
   
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  -- semlink <- loadSemLink (cfg^.cfg_semlink_file)
  -- let semlinkmap = createVNFNDB semlink

  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)

  ws <- loadStatistics (cfg^.cfg_statistics)
  
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (constituency .~ True)
                  )

    -- mapM_ (sentStructure pp . (^._3) ) ordered
    -- sentStructure pp txt
    queryProcess pp sensemap sensestat
