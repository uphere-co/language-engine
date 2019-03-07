{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Parser where

import           Data.Text                             (Text)
import           Control.Applicative                   ((<|>))
import           Data.Attoparsec.Text
import qualified Data.Text                     as T


import           WikiEL.Type


{-!
  Attoparsec parsers for various types. 
  In most cases, it will be simpler to use wrapping functions defined below.
-}

parserWikidataItemID :: Parser ItemID
parserWikidataItemID = do
  string "Q"
  i <- decimal
  return (QID i)


parserBrokenItemID :: Parser ItemID
parserBrokenItemID = do
  string "Q" <|> string "q"
  i <- decimal
  return (QID i)


parserWikidataPropertyID :: Parser PropertyID
parserWikidataPropertyID = do
  string "P"
  i <- decimal
  return (PropertyID i)


parserWikipediaPageID :: Parser PageID
parserWikipediaPageID = do
  i <- decimal
  return (PageID i)


column :: Parser Text
column = takeTill (== '\t')


sep :: Parser Text
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
  pageid <- parserWikipediaPageID
  sep
  itemid <- parserWikidataItemID  
  return (name, GICS gics, GICSsub gicsSub, Symbol symbol, pageid, itemid)


getParseResult :: Parser a -> Text -> a
getParseResult parser input = f (parseOnly parser input) 
  where
    f (Right r)   = r
    f (Left msg ) = error ("Error : " ++ msg ++ " from input, " ++ T.unpack input)


parseItemID :: Text -> Either String ItemID
parseItemID = parseOnly parserWikidataItemID 

parsePageID :: Text -> Either String PageID
parsePageID = parseOnly parserWikipediaPageID 

parserWordNetSynsetYAGO :: Parser SynsetY
parserWordNetSynsetYAGO = do
  tokens  <-  takeWhile1 (/= '_') `sepBy` string "_"
  let
    f acc (x:[a]) = (reverse (x:acc), a)
    f acc (x:xs)    = f (x:acc) xs
    f _acc [] = error "parserWordNetSynsetYAGO"
    (ws, idxStr) = f [] tokens
    Right idx = parseOnly decimal idxStr
  return (SynsetY (T.intercalate "_" ws) idx)


parserWordNetSynset :: Parser Synset
parserWordNetSynset = do
  string "synset-"
  tokens  <-  takeWhile1 (/= '-') `sepBy` string "-"
  let
    f acc (x:[a,b]) = (reverse (x:acc), a, b)
    f acc (x:xs)    = f (x:acc) xs
    f _acc [] = error "parserWordNetSynset"
    (ws, pos, idxStr) = f [] tokens
    Right idx = parseOnly decimal idxStr
  return (Synset (T.intercalate "-" ws) pos idx)


parserWordNetSynsetRow :: Parser WordNetMappingRow
parserWordNetSynsetRow = do
  title   <- column
  sep
  pageid <- parserWikipediaPageID
  sep
  itemid <- parserBrokenItemID  
  sep
  synset <- parserWordNetSynset
  return (WordNetMappingRow title pageid itemid synset)


parserWikiTitleMappingRow :: Parser WikiTitleMappingRow
parserWikiTitleMappingRow = do
  itemid <- parserBrokenItemID  
  sep
  title   <- column
  return (itemid, title)
  

{-!
  Wrapping functions of above parsers for ease of usages.
-}
  
itemID :: Text -> ItemID
itemID = getParseResult parserWikidataItemID

propertyID :: Text -> PropertyID
propertyID = getParseResult parserWikidataPropertyID

pageID :: Text -> PageID
pageID = getParseResult parserWikipediaPageID

wordnetSynset :: Text -> Synset
wordnetSynset = getParseResult parserWordNetSynset

wordnetSynsetYAGO :: Text -> SynsetY
wordnetSynsetYAGO = getParseResult parserWordNetSynsetYAGO

subclassRelation :: Text -> SubclassRelationRow
subclassRelation = getParseResult parserSubclassRelation

publicCompany :: Text -> (Text, GICS, GICSsub, Symbol, PageID, ItemID)
publicCompany = getParseResult parserPublicCompanyLine

propertyName :: Text -> PropertyNameRow
propertyName = getParseResult parserPropertyName

entityRepr :: Text -> EntityReprRow
entityRepr = getParseResult parserEntityRepr

wordNetMapping :: Text -> WordNetMappingRow
wordNetMapping = getParseResult parserWordNetSynsetRow

wikititleMapping :: Text -> WikiTitleMappingRow
wikititleMapping = getParseResult parserWikiTitleMappingRow
