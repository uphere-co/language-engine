{-# LANGUAGE OverloadedStrings #-}

module WordNet.Parser.Index where

import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           WordNet.Parser.Common
import           WordNet.Type
import           WordNet.Type.Lexicographer      (LexID(..))
import           WordNet.Type.POS


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
      synset_offset <- zip (map SenseNumber [1..]) <$> mapM (fmap SynsetOffset <$> readDecimal) r'
      return (IndexItem lem pos ptr_symbol tagsense_cnt synset_offset)
    worker _ = Nothing


parseSense :: Text -> Maybe SenseItem
parseSense = go . T.words
  where
    go (skey:soffset':snumber':cnt':[]) = do
      let lemmass:lexfilenum':lexid':headword:headid':[] = T.splitOn ":" skey
          lemma:ss':[] = T.splitOn "%" lemmass
      ss <- toEnum <$> readDecimal ss'
      lexfilenum <- toEnum <$> readDecimal lexfilenum'
      lexid <- LexID <$> readDecimal lexid'
      let headid = case (readDecimal headid') of
            Nothing -> LexID (-1)   -- why?
            Just n  -> LexID n
      soffset <- SynsetOffset <$> readDecimal soffset'
      snumber <- SenseNumber <$> readDecimal snumber'
      cnt <- readDecimal cnt'
      return $ SenseItem
                 (SenseKey lemma (LexSense ss lexfilenum lexid headword headid))
                 soffset
                 snumber
                 cnt
    go _ = Nothing



