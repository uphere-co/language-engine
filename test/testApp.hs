{-# LANGUAGE OverloadedStrings #-}

module Test.RDFDumpETL where

import           Data.Text                             (Text)
import           System.IO                             (Handle,stdin,stdout)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import           Data.Either                           (rights)

import           WikiEL.ETL.RDF


readBlocksImpl :: Handle -> (Text -> IO ()) -> Text -> IO ()
readBlocksImpl _ _ "" = return ()
readBlocksImpl handle fBlock prevBlock = do
  block <- T.IO.hGetChunk handle
  fBlock block
  readBlocksImpl handle fBlock block

readBlocks :: Handle -> (Text -> IO ()) -> IO ()
readBlocks stdin f = readBlocksImpl stdin f dummy
  where dummy = "asdf"

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
