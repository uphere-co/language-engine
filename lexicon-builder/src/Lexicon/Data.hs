{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Lexicon.Data where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Binary                as Bi
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List
import           Data.Text                          (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified Data.Text.Lazy.IO          as T.L.IO
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           Text.Taggy.Lens
--
import           FrameNet.Query.LexUnit
import           FrameNet.Type.LexUnit
import           VerbNet.Parser.SemLink
import           VerbNet.Type.SemLink
import           WordNet.Query
--
import           OntoNotes.Corpus.Load
import           OntoNotes.Parser.SenseInventory
import           OntoNotes.Type.SenseInventory


data LexDataConfig = LexDataConfig { _cfg_sense_inventory_file :: FilePath
                                   , _cfg_semlink_file         :: FilePath
                                   , _cfg_statistics           :: FilePath
                                   , _cfg_wsj_directory        :: FilePath
                                   , _cfg_framenet_lubin       :: FilePath
                                   , _cfg_framenet_framedir    :: FilePath
                                   , _cfg_wordnet_dict         :: FilePath
                                   , _cfg_propbank_framedir    :: FilePath
                                   , _cfg_wsj_corenlp_directory :: FilePath
                                   , _cfg_rolemap_file         :: FilePath
                                   , _cfg_idiom_file           :: FilePath
                                   , _cfg_verb_subcat_file     :: FilePath
                                   , _cfg_ukb_binfile          :: FilePath
                                   , _cfg_ukb_dictfile         :: FilePath
                                   , _cfg_company_file         :: FilePath
                                   , _cfg_wiki_dir             :: FilePath
                                   }
                   deriving (Show,Eq,Ord,Generic)

makeLenses ''LexDataConfig

instance FromJSON LexDataConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 5}

instance ToJSON LexDataConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 5}


loadLexDataConfig :: FilePath -> IO (Either String (LexDataConfig))
loadLexDataConfig fp = eitherDecode' <$> BL.readFile fp


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
  let lst = Bi.decode bstr :: [LexUnit]
      lexunitdb = foldl' insertLU emptyDB lst
  return lexunitdb

  
createVNFNDB :: VNFNMap -> HashMap (Text,Text) [Text]
createVNFNDB semlink = 
  let lst = map (\c-> ((c^.vnc_vnmember,c^.vnc_class),c^.vnc_fnframe)) (semlink^.vnfnmap_vnfns)
  in foldl' (\(!acc) (k,v) -> HM.insertWith (++) k [v] acc) HM.empty lst




loadAllexceptPropBank :: LexDataConfig -> IO (LexUnitDB, HashMap (Text,Text) Int, HashMap (Text,Text) [Text], HashMap Text Inventory, [(Text,Text)], WordNetDB)
loadAllexceptPropBank cfg = do
  ludb <- loadFrameNet (cfg^.cfg_framenet_lubin)
  sensestat <- senseInstStatistics (cfg^.cfg_wsj_directory)
  semlink <- loadSemLink (cfg^.cfg_semlink_file)
  let semlinkmap = createVNFNDB semlink
  sis <- loadSenseInventory (cfg^.cfg_sense_inventory_file)
  let sensemap = HM.fromList (map (\si -> (si^.inventory_lemma,si)) sis)
  wordstat <- loadStatistics (cfg^.cfg_statistics)
  wndb <- loadDB (cfg^.cfg_wordnet_dict)
  return (ludb,sensestat,semlinkmap,sensemap,wordstat,wndb)  

