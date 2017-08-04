{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.App.Load where

import           Control.Lens
import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Text                          (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified Data.Text.Lazy.IO          as T.L.IO
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           Text.Taggy.Lens
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.Frame    hiding (LexUnit)  
import           FrameNet.Type.LexUnit
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink
import           WordNet.Query
--
import           OntoNotes.Corpus.Load
import           OntoNotes.Parser.Sense
import           OntoNotes.Parser.SenseInventory
import           OntoNotes.Type.Sense
import           OntoNotes.Type.SenseInventory


data Config = Config { _cfg_sense_inventory_file :: FilePath
                     , _cfg_semlink_file         :: FilePath
                     , _cfg_statistics           :: FilePath
                     , _cfg_wsj_directory        :: FilePath
                     , _cfg_framenet_lubin       :: FilePath
                     , _cfg_framenet_framedir    :: FilePath
                     , _cfg_wordnet_dict         :: FilePath
                     }

makeLenses ''Config

              
cfg = Config { _cfg_sense_inventory_file = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/metadata/sense-inventories"
             , _cfg_semlink_file         = "/scratch/wavewave/SemLink/1.2.2c/vn-fn/VNC-FNF.s"
             , _cfg_statistics           = "/scratch/wavewave/run/20170717/OntoNotes_propbank_statistics_only_wall_street_journal_verbonly.txt"
             , _cfg_wsj_directory        = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"
             , _cfg_framenet_lubin       = "/scratch/wavewave/run/20170717/FrameNet_ListOfLexUnit.bin"
             , _cfg_framenet_framedir    = "/scratch/wavewave/FrameNet/1.7/fndata/fndata-1.7/frame"
             , _cfg_wordnet_dict         = "/scratch/wavewave/wordnet/WordNet-3.0/dict"
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


loadSemLink file = do
  txt <- T.L.IO.readFile file
  case txt ^? html . allNamed (only "verbnet-framenet_MappingData") of
    Nothing -> error "nothing"
    Just f -> case p_vnfnmappingdata f of
                Left err -> error err
                Right c -> return c

  
loadStatistics file = do
  txt <- T.IO.readFile file
  return $ map ((\(l:_:f:_) -> (l,f)) . T.words) (T.lines txt)


loadFrameNet file = do
  bstr <- BL.readFile file
  let lst = decode bstr :: [LexUnit]
      lexunitdb = foldl' insertLU emptyDB lst
  return lexunitdb
  
createVNFNDB :: VNFNMappingData -> HashMap (Text,Text) [Text]
createVNFNDB semlink = 
  let lst = map (\c-> ((c^.vnc_vnmember,c^.vnc_class),c^.vnc_fnframe)) (semlink^.vnfnmap_vnclslst)
  in foldl' (\(!acc) (k,v) -> HM.insertWith (++) k [v] acc) HM.empty lst





loadAll = do
  ludb <- loadFrameNet (cfg^.cfg_framenet_lubin)
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  semlink <- loadSemLink (cfg^.cfg_semlink_file)
  let semlinkmap = createVNFNDB semlink

  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  wordstat <- loadStatistics (cfg^.cfg_statistics)

  wndb <- loadDB (cfg^.cfg_wordnet_dict)

  return (ludb,sensestat,semlinkmap,sensemap,wordstat,wndb)  

