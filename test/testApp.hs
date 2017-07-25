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


f :: Text -> IO ()
f block = do
  let
    lines = T.lines block
    aliases = map (hasWikiAlias.readlineYAGO) lines
    synsets = filter (isWordNet.readlineYAGO)    lines
  --mapM_ print (rights aliases)
  mapM_ T.IO.putStrLn synsets


g :: ParsingState -> Text -> IO ParsingState
g prevState block = do
  let
    lines = T.lines block
    rs    = map readlineWikidata lines
    (state,ts)    = flattenStatementStream prevState rs
  --mapM_ print rs
  mapM_ print ts
  return state
  --mapM_ T.IO.putStrLn rs
  


main1 = readBlocks stdin f
main2 = readBlocks2 stdin g initState

main :: IO ()
main = main2
