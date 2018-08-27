{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module SRL.Analyze.Config where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON(..),ToJSON(..)
                  ,defaultOptions,fieldLabelModifier
                  ,genericParseJSON,genericToJSON)
import GHC.Generics (Generic)

data SRLConfig = SRLConfig {
                   _srlcfg_framenet_framedir    :: FilePath
                 , _srlcfg_wordnet_dict         :: FilePath
                 , _srlcfg_wsj_directory        :: FilePath
                 , _srlcfg_sense_inventory_file :: FilePath
                 , _srlcfg_rolemap_file         :: FilePath
                 , _srlcfg_idiom_file           :: FilePath
                 , _srlcfg_verb_subcat_file     :: FilePath
                 , _srlcfg_ukb_binfile          :: FilePath
                 , _srlcfg_ukb_dictfile         :: FilePath
                 , _srlcfg_wiki_dir             :: FilePath
                 , _srlcfg_company_file         :: FilePath
                 }
               deriving (Show,Generic)

makeLenses ''SRLConfig

instance FromJSON SRLConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 8}

instance ToJSON SRLConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 8}


data Config = Config { _showDetail     :: Bool
                     , _showFullDetail :: Bool
                     , _bypassNER      :: Bool
                     , _bypassTEXTNER  :: Bool
                     , _configFile     :: FilePath
                     }
            deriving (Show)


makeLenses ''Config

