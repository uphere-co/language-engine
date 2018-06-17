{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.TaggedText where

import           Control.Lens.TH       (makeLenses)
import           Data.Text             (Text)

data SUTime = SUTime
  { _sutime_value :: Maybe Text
  , _sutime_index :: [Int]
  } deriving (Show)

makeLenses ''SUTime

data MetaInfo a = MetaInfo { _metainfo_info :: a } deriving (Show)

makeLenses ''MetaInfo

data TagInfo a = TagInfo
  { _taginfo_range :: (Int,Int)
  , _taginfo_metainfo :: Maybe (MetaInfo a)
  , _taginfo_text :: Text
  } deriving (Show)

makeLenses ''TagInfo

data TaggedText a = TaggedText { _tagged_text :: Text, _tagged_taginfos :: [TagInfo a] } deriving (Show)

makeLenses ''TaggedText

