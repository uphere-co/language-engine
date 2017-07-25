{-# LANGUAGE OverloadedStrings #-}

module Test.RDFDumpETL where

import           Data.Text                             (Text)
import           System.IO                             (stdin,stdout)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import           Data.Either                           (rights)

import           WikiEL.ETL.RDF
import           WikiEL.ETL.Util



hasWikiAlias (Right (_,ts,tv@(YagoVerb v),to@(YagoWikiAlias _))) | v =="redirectedFrom" = Right (ts, to)
hasWikiAlias (Right _) = Left "Not English Wikipedia redirects."
hasWikiAlias (Left msg) = Left msg

isWordNet (Right (_,ts,tv,to@(YagoWordnet _))) = True
isWordNet _ = False


yago :: Text -> Text -> IO Text
yago prevPartialBlock block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    lines = T.lines (T.append prevPartialBlock mainBlock)
    aliases = map (hasWikiAlias.readlineYAGO) lines
    synsets = filter (isWordNet.readlineYAGO) lines
  --mapM_ print (rights aliases)
  mapM_ T.IO.putStrLn synsets
  return partialBlock


wikidata :: (ParsingState,Text) -> Text -> IO (ParsingState,Text)
wikidata (prevState, prevPartialBlock) block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    lines = T.lines (T.append prevPartialBlock mainBlock)
    rs    = map readlineWikidata lines
    (state,ts)    = flattenStatementStream prevState rs
  --mapM_ print rs
  mapM_ print ts
  return (state,partialBlock)
  --mapM_ T.IO.putStrLn rs
  


main1 = readBlocks stdin yago ""
main2 = readBlocks stdin wikidata (initState, "")

main :: IO ()
main = main1
