{-# LANGUAGE OverloadedStrings #-}

module Test.RDFDumpETL where

import           Data.Text                             (Text)
import           Data.Maybe                            (mapMaybe)
import           System.IO                             (stdin,stdout)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import           Data.Either                           (rights)

import           WikiEL.ETL.RDF
import           WikiEL.ETL.Util


hasWikiAlias :: Either a YagoRdfTriple -> Maybe (YagoObject, YagoObject)
hasWikiAlias (Right (_,ts,tv@(YagoVerb v),to@(YagoWikiAlias _))) | v =="redirectedFrom" = Just (ts, to)
hasWikiAlias _ = Nothing

isWordNet :: Either a YagoRdfTriple -> Bool
isWordNet (Right (_,ts,tv,to@(YagoWordnet _))) = True
isWordNet _ = False

wikicatOfWordNetT :: Either a YagoRdfTriple -> Maybe Text
wikicatOfWordNetT (Right (_,ts@(YagoWikicat cat),tv,to@(YagoWordnet synset)) ) = x
  where x = Just (T.concat [cat, "\t",synset])
wikicatOfWordNetT _ = Nothing

yago :: Text -> Text -> IO Text
yago prevPartialBlock block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    lines = T.lines (T.append prevPartialBlock mainBlock)
    aliases = map (hasWikiAlias.readlineYAGO) lines
    synsets = filter (isWordNet.readlineYAGO) lines
    typedCats = mapMaybe (wikicatOfWordNetT.readlineYAGO) lines
  --mapM_ print (rights aliases)
  --mapM_ T.IO.putStrLn synsets
  mapM_ T.IO.putStrLn typedCats
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
main = main2
