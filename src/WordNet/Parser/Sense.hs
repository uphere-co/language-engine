{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Sense where

import           Data.Text           (Text)
import qualified Data.Text    as T
--
import           WordNet.Type
import           WordNet.Parser.Common
--
  
parseSense :: Text -> Maybe SenseItem
parseSense = go . T.words
  where
    go (skey:soffset':snumber':cnt':[]) = do
      let lemmass:lexfilenum':lexid':headword:headid':[] = T.splitOn ":" skey
          lemma:ss':[] = T.splitOn "%" lemmass
      ss <- toEnum <$> readDecimal ss'
      lexfilenum <- toEnum <$> readDecimal lexfilenum'
      lexid <- readDecimal lexid'
      let headid = case (readDecimal headid') of
            Nothing -> -1
            Just n  -> n
      soffset <- readDecimal soffset'
      snumber <- readDecimal snumber'
      cnt <- readDecimal cnt'
      return $ SenseItem
                 (SenseKey lemma (LexSense ss lexfilenum lexid headword headid))
                 soffset
                 snumber
                 cnt
    go _ = Nothing


