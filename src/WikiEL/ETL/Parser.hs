{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.Parser where

import           Data.Text                             (Text)
import           Data.Attoparsec.Text

import           WikiEL.Types.Wikidata
import           WikiEL.Types.Wikipedia
import           WikiEL.Types.Equity


parserWikidataItemID :: Parser ItemID
parserWikidataItemID = do
  string "Q"
  id <- decimal
  return (ItemID id)

parserWikipediaPageID :: Parser PageID
parserWikipediaPageID = do
  id <- decimal
  return (PageID id)


column = takeTill (== '\t')
sep = string "\t"

parserSubclassRelation :: Parser (ItemID, ItemID)
parserSubclassRelation = do
  super <- parserWikidataItemID
  sep
  _   <- column -- no use for super_title
  sep
  sub <- parserWikidataItemID
  sep
  _   <- column -- no use for sub_title
  return (super, sub)

parserPublicCompanyLine :: Parser (Text, GICS, GICSsub, Symbol, PageID, ItemID)
parserPublicCompanyLine = do
  name   <- column
  sep
  symbol <- column
  sep
  gics   <- column
  sep
  gicsSub <- column  
  sep
  pageID <- parserWikipediaPageID
  sep
  itemID <- parserWikidataItemID  
  return (name, GICS gics, GICSsub gicsSub, Symbol symbol, pageID, itemID)

getParseResult :: Parser a -> Text -> a
getParseResult parser input = f (parseOnly parser input) 
  where
    f (Right r)   = r
    f (Left msg ) = error ("Error : "++msg)


parseItemID :: Text -> Either String ItemID
parseItemID = parseOnly parserWikidataItemID 

parsePageID :: Text -> Either String PageID
parsePageID = parseOnly parserWikipediaPageID 


itemID :: Text -> ItemID
itemID itemID = getParseResult parserWikidataItemID itemID

pageID :: Text -> PageID
pageID itemID = getParseResult parserWikipediaPageID itemID

subclassRelation :: Text -> (ItemID, ItemID)
subclassRelation line = getParseResult parserSubclassRelation line

publicCompany :: Text -> (Text, GICS, GICSsub, Symbol, PageID, ItemID)
publicCompany line = getParseResult parserPublicCompanyLine line
