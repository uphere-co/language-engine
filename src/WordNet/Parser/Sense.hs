{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Sense where

import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text    as T
--
import           WordNet.Type
import           WordNet.Parser.Common
--
import           NLP.Type.WordNet
  
parseSense :: Text -> Maybe SenseItem
parseSense = worker . T.words
  where
    worker (skey:soffset':snumber':cnt':[]) = do
      let lemmass:lexfilenum':lexid':headword:headid':[] = T.splitOn ":" skey
          lemma:ss':[] = T.splitOn "%" lemmass
      ss <- readDecimal ss'
      lexfilenum <- readDecimal lexfilenum'
      lexid <- readDecimal lexid'
      let headid = case (readDecimal headid') of
            Nothing -> -1
            Just n  -> n
      soffset <- readDecimal soffset'
      snumber <- readDecimal snumber'
      cnt <- readDecimal cnt'
      return (SenseItem lemma ss lexfilenum lexid headword headid soffset snumber cnt)
    worker _ = Nothing


