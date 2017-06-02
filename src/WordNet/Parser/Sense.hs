{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Sense where

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
      let [ssfilename,lexid',headword,headid'] = T.splitOn ":" skey
          [ss',filename] = T.splitOn "%" ssfilename
      ss <- readDecimal ss'
      lexid <- readDecimal lexid'
      headid <- readDecimal headid'
      soffset <- readDecimal soffset'
      snumber <- readDecimal snumber'
      cnt <- readDecimal cnt'
      return (SenseItem ss filename lexid headword headid soffset snumber cnt)
    worker _ = Nothing


