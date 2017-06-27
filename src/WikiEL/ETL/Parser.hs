{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.Parser where

import           Data.Text                             (Text)
import           Data.Attoparsec.Text

import           WikiEL.Types.Wikidata
import           WikiEL.Types.Wikipedia
import           WikiEL.Types.Equity
import           WikiEL.Types.FileFormat


parserWikidataItemID :: Parser ItemID
parserWikidataItemID = do
  string "Q"
  id <- decimal
  return (ItemID id)

parserWikidataPropertyID :: Parser PropertyID
parserWikidataPropertyID = do
  string "P"
  id <- decimal
  return (PropertyID id)

parserWikipediaPageID :: Parser PageID
parserWikipediaPageID = do
  id <- decimal
  return (PageID id)


column = takeTill (== '\t')
sep = string "\t"

parserPropertyName :: Parser PropertyNameRow
parserPropertyName = do
  prop <- parserWikidataPropertyID
  sep
  name <- column
  return (PropertyNameRow prop name)

parserEntityRepr :: Parser EntityReprRow
parserEntityRepr = do
  item <- parserWikidataItemID
  sep
  repr <- column
  return (EntityReprRow item (ItemRepr repr))




parserSubclassRelation :: Parser SubclassRelationRow
parserSubclassRelation = do
  sub <- parserWikidataItemID
  sep
  _   <- column -- no use for sub_title
  sep
  super <- parserWikidataItemID
  sep
  _   <- column -- no use for super_title
  return (SubclassRelationRow sub super)

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
itemID = getParseResult parserWikidataItemID

propertyID :: Text -> PropertyID
propertyID = getParseResult parserWikidataPropertyID

pageID :: Text -> PageID
pageID = getParseResult parserWikipediaPageID


subclassRelation :: Text -> SubclassRelationRow
subclassRelation = getParseResult parserSubclassRelation

publicCompany :: Text -> (Text, GICS, GICSsub, Symbol, PageID, ItemID)
publicCompany = getParseResult parserPublicCompanyLine

propertyName :: Text -> PropertyNameRow
propertyName = getParseResult parserPropertyName

entityRepr :: Text -> EntityReprRow
entityRepr = getParseResult parserEntityRepr