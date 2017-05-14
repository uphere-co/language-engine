{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Index where

import           Control.Lens
import           Data.Text           (Text)
import qualified Data.Text    as T
import           Data.Text.Read
--
import           WordNet.Type
import           WordNet.Parser.Common

parseIndex :: Text -> Maybe IndexItem
parseIndex = worker . T.words
  where
    worker (lem:pos:cnt1:cnt2:rem) = do
      synset_cnt <- readDecimal cnt1
      p_cnt <- readDecimal cnt2
      let (ptr_symbol,cnt3:cnt4:rem') = splitAt p_cnt rem
      sense_cnt <- readDecimal cnt3
      tagsense_cnt <- readDecimal cnt4
      synset_offset <- mapM readDecimal rem'
      return (IndexItem lem pos synset_cnt p_cnt ptr_symbol sense_cnt tagsense_cnt synset_offset)
    worker _ = Nothing


