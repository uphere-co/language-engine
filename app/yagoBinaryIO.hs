{-# LANGUAGE OverloadedStrings #-}

import           Data.Text                             (Text)
import           Data.Maybe                            (mapMaybe,catMaybes)
import           System.IO                             (stdin,stdout)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO


import           WikiEL.ETL.RDF
import           WikiEL.ETL.Util
import           WikiEL.ETL.RDF.Binary
import           WikiEL.Type.WordNet                   (SynsetY)
import           WikiEL.ETL.Parser                     (wordnetSynsetYAGO)

yagoBinarySave :: IO ()
yagoBinarySave = do
  let
    input_text = "yago/wordnet"
    --input_text = "wordnet"
  lines <- readlines input_text
  let
    rows = mapParsed readlineYAGO lines
    --bin = encode rows
  --mapM_ print (decodeYagoRDF bin)
  --encodeFile "yago/sample.bin" rows
  print (length rows)
  encodeFile (input_text ++ ".bin") rows
  
yagoBinaryLoad :: IO ()
yagoBinaryLoad = do
  --rows <- decodeYAGOFile "yago/sample.bin"
  --rows <- decodeYAGOFile "wordnet.bin"
  rows <- decodeYAGOFile "yago/wordnet.bin"
  print (length rows)
  

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

wordnetTypes :: Either a YagoRdfTriple -> Maybe Text
wordnetTypes (Right (_,ts@(YagoWikiTitle s),_,to@(YagoWordnet o))) = Just (T.intercalate "\t" [s,o])
wordnetTypes _ = Nothing

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
    wnTypes = mapMaybe (wordnetTypes . readlineYAGO) lines
    --links = mapMaybe (interWikiLinks . parseUserDefined parserInterEnwikiLinks) lines
  --mapM_ print (rights aliases)
  --mapM_ T.IO.putStrLn synsets
  --mapM_ T.IO.putStrLn typedCats
  --mapM_ print taxons  
  --mapM_ T.IO.putStrLn links
  mapM_ T.IO.putStrLn wnTypes
  
  return partialBlock

main1 = readBlocks stdin yago ""


main :: IO ()
--main = yagoBinarySave
--main = yagoBinaryLoad
main = main1

