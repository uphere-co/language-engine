{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module OntoNotes.App.Load where

import           Control.Lens
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.Function                      (on)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Text                          (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified Data.Text.Lazy.IO          as T.L.IO
import           Data.Text.Read                   (decimal)
import           System.Directory
import           System.FilePath
import           Text.Taggy.Lens
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.LexUnit
import           Lexicon.Type
import           NLP.Syntax.Type
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink
import           WordNet.Query
--
import           OntoNotes.Corpus.Load
import           OntoNotes.Parser.SenseInventory
import           OntoNotes.Type.SenseInventory


data Config = Config { _cfg_sense_inventory_file :: FilePath
                     , _cfg_semlink_file         :: FilePath
                     , _cfg_statistics           :: FilePath
                     , _cfg_wsj_directory        :: FilePath
                     , _cfg_framenet_lubin       :: FilePath
                     , _cfg_framenet_framedir    :: FilePath
                     , _cfg_wordnet_dict         :: FilePath
                     , _cfg_propbank_framedir    :: FilePath
                     , _cfg_wsj_corenlp_directory :: FilePath
                     , _cfg_rolemap_file         :: FilePath
                     , _cfg_verb_subcat_file     :: FilePath
                     }

makeLenses ''Config

cfg :: Config              
cfg = Config { _cfg_sense_inventory_file = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
             , _cfg_semlink_file         = "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VNC-FNF.s"
             , _cfg_statistics           = "/scratch/wavewave/run/20170717/OntoNotes_propbank_statistics_only_wall_street_journal_verbonly.txt"
             , _cfg_wsj_directory        = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
             , _cfg_framenet_lubin       = "/scratch/wavewave/run/20170717/FrameNet_ListOfLexUnit.bin"
             , _cfg_framenet_framedir    = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7/frame"
             , _cfg_wordnet_dict         = "/scratch/wavewave/wordnet/WordNet-3.0/dict"
             , _cfg_propbank_framedir    = "/home/wavewave/repo/srcc/propbank-frames/frames"
             , _cfg_wsj_corenlp_directory = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
             , _cfg_rolemap_file         = "/home/wavewave/repo/srcp/OntoNotes/mapping/final.txt"
             , _cfg_verb_subcat_file     = "/scratch/wavewave/run/20170820/verbsubcat_propbank_ontonotes_statonly.tsv"
             }

-- | This is the global config.
--
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
              , _cfg_rolemap_file          = "/home/modori/repo/src/OntoNotes/mapping/final.txt"
              , _cfg_verb_subcat_file      = "/data/groups/uphere/data/NLP/run/20170817/verbsubcat_propbank_ontonotes_statonly.tsv"
              }


loadSenseInventory :: FilePath -> IO [Inventory]
loadSenseInventory dir = do
  cnts <- getDirectoryContents dir
  let fs = sort (filter (\x -> takeExtensions x == ".xml") cnts)
  flip traverse fs $ \f -> do
    let fp = dir  </> f
    txt <- T.L.IO.readFile fp
    case txt ^? html . allNamed (only "inventory") of
      Nothing -> error "nothing"
      Just x -> case p_inventory x of
                  Left e -> error e
                  Right c  -> return c


loadSemLink :: FilePath -> IO VNFNMap
loadSemLink fp = do
  txt <- T.L.IO.readFile fp
  case txt ^? html . allNamed (only "verbnet-framenet_MappingData") of
    Nothing -> error "nothing"
    Just x -> case p_vnfnmap x of
                Left e -> error e
                Right c -> return c


loadStatistics :: FilePath -> IO [(Text,Text)]
loadStatistics fp = do
  txt <- T.IO.readFile fp
  return $ map ((\(l:_:f:_) -> (l,f)) . T.words) (T.lines txt)


loadFrameNet :: FilePath -> IO LexUnitDB
loadFrameNet fp = do
  bstr <- BL.readFile fp
  let lst = decode bstr :: [LexUnit]
      lexunitdb = foldl' insertLU emptyDB lst
  return lexunitdb

  
createVNFNDB :: VNFNMap -> HashMap (Text,Text) [Text]
createVNFNDB semlink = 
  let lst = map (\c-> ((c^.vnc_vnmember,c^.vnc_class),c^.vnc_fnframe)) (semlink^.vnfnmap_vnfns)
  in foldl' (\(!acc) (k,v) -> HM.insertWith (++) k [v] acc) HM.empty lst




loadAllexceptPropBank :: IO (LexUnitDB, HashMap (Text,Text) Int, HashMap (Text,Text) [Text], HashMap Text Inventory, [(Text,Text)], WordNetDB)
loadAllexceptPropBank = do
  ludb <- loadFrameNet (cfg^.cfg_framenet_lubin)
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  semlink <- loadSemLink (cfg^.cfg_semlink_file)
  let semlinkmap = createVNFNDB semlink
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  wordstat <- loadStatistics (cfg^.cfg_statistics)
  wndb <- loadDB (cfg^.cfg_wordnet_dict)
  return (ludb,sensestat,semlinkmap,sensemap,wordstat,wndb)  

