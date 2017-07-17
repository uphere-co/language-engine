{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.RDF.Common where


import           Data.Char                             (isSpace)
import           Data.Text                             (Text,head)
import           Data.Attoparsec.Text
import qualified Data.Text                     as T


parserObject :: Text -> Text -> (Text -> a) -> Parser a
parserObject prefix postfix f = do
  let 
    g "" = not . isSpace
    g x  = (/= T.head x)
  string prefix
  t <- takeWhile1 (g postfix)
  string postfix
  return (f t)

parserObject2 :: Text -> Text -> Text -> (Text -> Text -> a) -> Parser a
parserObject2 prefix sep postfix f = do
  let 
    g "" = not . isSpace
    g x  = (/= T.head x)
  string prefix
  t1 <- takeWhile1 (g sep)
  string sep
  t2 <- takeWhile1 (g postfix)
  string postfix
  return (f t1 t2)

parserTypedValue   :: (Text -> Text -> a) -> Parser a
parserTypedValue  = parserObject2 "\"" "\"^^" ""
