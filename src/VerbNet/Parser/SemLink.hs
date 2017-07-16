{-# LANGUAGE OverloadedStrings #-}

module VerbNet.Parser.SemLink where

import           Control.Applicative
import           Control.Lens       hiding (element,elements)
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Traversable          (traverse)
import           Text.Taggy.Lens
import           System.Directory
import           System.FilePath
--
import           Text.Taggy.Lens.Util
--
import           VerbNet.Type.SemLink


p_vnclass :: Element -> Parser VNClass
p_vnclass x = VNClass <$> x .: "class"
                      <*> x .: "vnmember"
                      <*> x .: "fnframe"
                      <*> x .: "fnlexent"
                      <*> x .: "versionID"

p_vnfnmappingdata :: Element -> Parser VNFNMappingData
p_vnfnmappingdata x = VNFNMappingData <$> mapM p_vnclass (getOnly x "vncls")
                                      <*> x .: "date"
