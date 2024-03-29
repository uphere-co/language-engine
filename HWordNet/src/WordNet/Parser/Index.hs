{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Index where

import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           WordNet.Parser.Common
import           WordNet.Type
import           WordNet.Type.Lexicographer      (LexID(..))


parseIndex :: Text -> Maybe IndexItem
parseIndex = worker . T.words
  where
    worker (lem:pos':_:cnt2:r) = do
      pos <- readPOS (T.head pos')
      p_cnt <- readDecimal cnt2
      let (ptr_symbol,_:cnt4:r') = splitAt p_cnt r
      tagsense_cnt <- readDecimal cnt4
      synset_offset <- zip (map SenseNumber [1..]) <$> mapM (fmap SynsetOffset <$> readDecimal) r'
      return (IndexItem lem pos ptr_symbol tagsense_cnt synset_offset)
    worker _ = Nothing


parseSense :: Text -> Maybe SenseItem
parseSense = go . T.words
  where
    go (skey:soffset':snumber':cnt':[]) = do
      let lemmass:lexfilenum':lexid':headword':headid':[] = T.splitOn ":" skey
          lemma:ss':[] = T.splitOn "%" lemmass
      ss <- toEnum <$> readDecimal ss'
      lexfilenum <- toEnum <$> readDecimal lexfilenum'
      let lexid = LexID (T.head lexid')
      let headword = if T.null headword' then Nothing else Just headword'
      let headid = if T.null headid' then Nothing else Just (LexID (T.head headid'))
      soffset <- SynsetOffset <$> readDecimal soffset'
      snumber <- SenseNumber <$> readDecimal snumber'
      cnt <- readDecimal cnt'
      return $ SenseItem
                 (SenseKey lemma (LexSense ss lexfilenum lexid headword headid))
                 soffset
                 snumber
                 cnt
    go _ = Nothing



