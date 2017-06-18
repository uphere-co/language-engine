{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Query.LexUnit where

import           Control.Lens
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.Text                    (Text)
import qualified Data.Text.Lazy.IO    as TLIO
import           System.FilePath              ((</>),(<.>))
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.LexUnit
import           HFrameNet.Type.LexUnit

newtype LexUnitDB = LexUnitDB { _frameDB :: HashMap Text LexUnit } 

makeLenses ''LexUnitDB



