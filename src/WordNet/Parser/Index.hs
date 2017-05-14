{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Index where

import           Data.Text           (Text)
import qualified Data.Text    as T
--
import           WordNet.Type
import           WordNet.Parser.Common


readPOS :: Char -> Maybe POS
readPOS 'n' = Just POS_N
readPOS 'v' = Just POS_V
readPOS 'a' = Just POS_A
readPOS 'r' = Just POS_R
readPOS _   = Nothing
  
parseIndex :: Text -> Maybe IndexItem
parseIndex = worker . T.words
  where
    worker (lem:pos':_:cnt2:r) = do
      pos <- readPOS (T.head pos')
      p_cnt <- readDecimal cnt2
      let (ptr_symbol,_:cnt4:r') = splitAt p_cnt r
      tagsense_cnt <- readDecimal cnt4
      synset_offset <- mapM readDecimal r'
      return (IndexItem lem pos ptr_symbol tagsense_cnt synset_offset)
    worker _ = Nothing


