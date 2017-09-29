{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Investopedia 

where

import           Data.Maybe                            (mapMaybe)
import           Data.Text                             (Text)
import qualified Data.Text.IO                  as T.IO
import qualified Data.Text                     as T
import qualified Data.List                     as L

import           WikiEL.ETL.Util                        (readlines)


{-|
  InvestopediaTerm is for representing page titles in Investopedia.
  The loadInvestopediaTagger is a text-match based tagger.
-}


newtype InvestopediaTerm = InvestopediaTerm Text
                         deriving (Show,Eq)
type InvestopediaTerms = [Text]

loadInvestopediaTerms :: FilePath -> IO InvestopediaTerms
loadInvestopediaTerms file = do
  lines <- readlines file
  let
    -- add a space to exclude substring matching cases.
    -- require length > 1 to remove some noise like "A", "B", and so on.
    terms = map (`T.snoc` ' ') (filter (\x -> T.length x >1) lines)
  return terms

-- tagging is just substring finding. Need to be refined later.
tagInvestopediaTerm :: InvestopediaTerms -> Text -> [InvestopediaTerm]
tagInvestopediaTerm terms text = mapMaybe f terms
  where
    f term | T.isInfixOf term text = Just (InvestopediaTerm term)
    f _ = Nothing

loadInvestopediaTagger :: FilePath -> IO (Text -> Maybe [InvestopediaTerm])
loadInvestopediaTagger file = do
  terms <- loadInvestopediaTerms file
  let
    tagger text = f tagged
      where
        tagged = tagInvestopediaTerm terms text
        f ts | L.null ts = Nothing
        f ts = Just ts
  return tagger

