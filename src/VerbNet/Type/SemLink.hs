{-# LANGUAGE TemplateHaskell #-}

module VerbNet.Type.SemLink where

import           Control.Lens
import           Data.Text
import           Text.Taggy.Lens

data VNClass = VNClass { _vnc_class :: Text
                       , _vnc_vnmember :: Text
                       , _vnc_fnframe :: Text
                       , _vnc_fnlexent :: Text
                       , _vnc_versionID :: Text
                       }
             deriving Show

makeLenses ''VNClass


data VNFNMappingData = VNFNMappingData { _vnfnmap_vnclslst :: [VNClass]
                                       , _vnfnmap_date :: Text
                                       }
                     deriving Show

makeLenses ''VNFNMappingData

                      
                      
