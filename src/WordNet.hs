{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet where

import Control.Lens
import           Data.Text           (Text)
import qualified Data.Text    as T
import           Data.Text.Read
--
import WordNet.Type

isComment :: Text -> Bool
isComment t | T.length t > 2 = T.take 2 t == "  "
            | otherwise    = True

eitherToMaybe (Left x) = Nothing
eitherToMaybe (Right x) = Just x

parse :: Text -> Maybe LexItem
parse = worker . T.words
  where
    worker (lem:pos:cnt1:cnt2:rem) = do
      synset_cnt <- fst <$> eitherToMaybe (decimal cnt1)
      p_cnt <- fst <$> eitherToMaybe (decimal cnt2)
      let (ptr_symbol,cnt3:cnt4:rem') = splitAt p_cnt rem
      sense_cnt <- fst <$> eitherToMaybe (decimal cnt3)
      tagsense_cnt <- fst <$> eitherToMaybe (decimal cnt4)
      synset_offset <- mapM (fmap fst . eitherToMaybe . decimal) rem'
      return (LexItem lem pos synset_cnt p_cnt ptr_symbol sense_cnt tagsense_cnt synset_offset)
    worker _ = Nothing


