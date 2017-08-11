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
import           WikiEL.ETL.Parser                     (wordnetSynsetYAGO)
import           WikiEL.Type.WordNet                   (SynsetY)

import           WikiEL.ETL.RDF.Yago

-- For Wiki interlinks
--import           Data.Text                             (Text)
import           Data.List                             (foldl')
import qualified Data.Map.Strict               as M
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector.Fusion.Bundle     as B
import qualified WikiEL.Util.Hash              as H
import qualified WikiEL.Graph                  as G


hasWikiAlias :: Either a YagoRdfTriple -> Maybe (YagoObject, YagoObject)
hasWikiAlias (Right (_,ts,tv@(YagoVerb v),to@(YagoWikiAlias _))) | v =="redirectedFrom" = Just (ts, to)
hasWikiAlias _ = Nothing

isWordNet :: Either a YagoRdfTriple -> Bool
isWordNet (Right (_,ts,tv,to@(YagoWordnet _))) = True
isWordNet _ = False

taxonomyWordNet :: Either a YagoRdfTriple -> Maybe (SynsetY, SynsetY)
taxonomyWordNet (Right (_,YagoWordnet sub,YagoRDFSprop p,YagoWordnet super))|p == "subClassOf" = Just (x, y)
  where
    x = wordnetSynsetYAGO sub
    y = wordnetSynsetYAGO super
taxonomyWordNet _ = Nothing

wikicatOfWordNetT :: Either a YagoRdfTriple -> Maybe Text
wikicatOfWordNetT (Right (_,ts@(YagoWikicat cat),tv,to@(YagoWordnet synset)) ) = x
  where x = Just (T.concat [cat, "\t",synset])
wikicatOfWordNetT _ = Nothing


interWikiLinks :: Either a YagoRdfTriple -> Maybe Text
interWikiLinks (Right (_,ts@(YagoWikiTitle s),_,to@(YagoWikiTitle o))) = Just (T.intercalate "\t" [s,o])
interWikiLinks _ = Nothing

{-
-- following got just ~10% speed-up compared to readlineYAGO.
parserInterEnwikiLinks :: Parser YagoRdfTriple
parserInterEnwikiLinks = do
  let ssep = skipWhile C.isSpace
  ssep
  subj <- parserYAGOwikiTitle
  ssep
  verb <- parserYAGOverb
  ssep
  obj  <- parserYAGOwikiTitle
  ssep
  return (nullID, subj, verb, obj)
parseUserDefined = parseOnly
-}

yago :: Text -> Text -> IO Text
yago prevPartialBlock block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    lines = T.lines (T.append prevPartialBlock mainBlock)
    aliases = map (hasWikiAlias.readlineYAGO) lines
    synsets = filter (isWordNet.readlineYAGO) lines
    typedCats = mapMaybe (wikicatOfWordNetT.readlineYAGO) lines

    taxons = mapMaybe (taxonomyWordNet.readlineYAGO) lines
    links = mapMaybe (interWikiLinks . readlineYAGO) lines
    --links = mapMaybe (interWikiLinks . parseUserDefined parserInterEnwikiLinks) lines
  --mapM_ print (rights aliases)
  --mapM_ T.IO.putStrLn synsets
  --mapM_ T.IO.putStrLn typedCats
  --mapM_ print taxons
  mapM_ T.IO.putStrLn links
  
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
  

type HashInvs =  M.Map H.WordHash Text
data Foo = Foo { _edges :: UV.Vector (H.WordHash, H.WordHash)
               , _names  :: HashInvs
               }
         deriving (Show)

initFoo = Foo UV.empty M.empty

printFoo foo@(Foo edges names) = do
  let 
    lookup key = M.lookup key names
  mapM_ (print . (\(x,y) -> (lookup x, lookup y))) (UV.toList edges)
  print names

tryAdd :: HashInvs -> Text -> HashInvs
tryAdd invs word = M.insert (H.wordHash word) word invs

parseInterlinks :: Text -> (Text, Text)
parseInterlinks line = (from, to)
  where
    [from,to] = T.words line

addEdge :: Foo -> (Text,Text) -> Foo
addEdge foo@(Foo edges names) (from,to) = foo'
  where
    edge = (H.wordHash from, H.wordHash to)
    foo' = Foo (UV.snoc edges edge) (tryAdd (tryAdd names from) to)


loadInterlinks :: (Foo,Text) -> Text -> IO (Foo,Text)
loadInterlinks (prevState, prevPartialBlock) block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    lines = T.lines (T.append prevPartialBlock mainBlock)
    edges    = map parseInterlinks lines
    state = foldl' addEdge prevState edges
  --mapM_ print edges
  return (state,partialBlock)

main1 = readBlocks stdin yago ""
main2 = readBlocks stdin wikidata (initState, "")
main3 = do
  (foo@(Foo edges names),_) <- readBlocks2 stdin loadInterlinks (initFoo, "")
  let
    dForwardEdges  = G.neighbor edges G.from
    tmp = G.allPathsUpto dForwardEdges 1079244021 1
  --printFoo foo
  --print "=================================="
  --mapM_ print (B.toList tmp)
  print $ dForwardEdges 1079244021
  print $ dForwardEdges (H.wordHash "Diemelsee")


main :: IO ()
main = main3

