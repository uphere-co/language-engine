{-# LANGUAGE TemplateHaskell #-}

module WordNet.Type where

import Control.Lens
import Data.Text (Text)

data IndexItem = IndexItem { _idx_lemma :: Text
                           , _idx_pos :: Text
                           , _idx_synset_cnt :: Int
                           , _idx_p_cnt :: Int
                           , _idx_ptr_symbol :: [Text]
                           , _idx_sense_cnt :: Int
                           , _idx_tagsense_cnt :: Int
                           , _idx_synset_offset :: [Int]
                           }
               deriving (Show)

makeLenses ''IndexItem


