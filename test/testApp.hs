{-# LANGUAGE OverloadedStrings #-}

module Test.RDFDumpETL where

import           Data.Text                             (Text)
import           System.IO                             (stdin,stdout)
import qualified Data.Text                     as T
import           Data.Either                           (rights)

import           WikiEL.ETL.RDF
import           WikiEL.ETL.Util



hasWikiAlias (_,ts,tv@(YagoVerb v),to@(YagoWikiAlias _)) | v =="redirectedFrom" = Right (ts, to)
hasWikiAlias _ = Left "Not English Wikipedia redirects."

hasWordNet (_,ts,tv,to@(YagoWordnet _)) = Right (ts, to)
hasWordNet _ = Left "Not WordNet synsets."


f :: Text -> IO ()
f block = do
  let
    lines = T.lines block
    rows = map readlineYAGO lines
    aliases = map hasWikiAlias (rights rows)
    synsets = map hasWordNet (rights rows)
  mapM_ print (rights aliases)
  mapM_ print (rights synsets)

main1 = readBlocks stdin f

main :: IO ()
main = main1
