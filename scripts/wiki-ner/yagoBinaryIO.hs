{-# LANGUAGE OverloadedStrings #-}

import           System.Environment                    (getArgs)
import           Data.Text                             (Text)
import           Data.Maybe                            (mapMaybe)
import           System.IO                             (stdin)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.ETL.RDF
import           WikiEL.ETL.Util
import           WikiEL.ETL.RDF.Binary
import           WikiEL.Type.WordNet                   (SynsetY)
import           WikiEL.ETL.Parser                     (wordnetSynsetYAGO)

-- yagoBinarySave and yagoBinaryLoad are rather experimental.
-- For now, a size of binary is too large and no noticeable performance gains.

yagoBinarySave :: IO ()
yagoBinarySave = do
  let
    input_text = "yago/wordnet"
    --input_text = "wordnet"
  ls <- readlines input_text
  let
    rows = mapParsed readlineYAGO ls
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

-- | returns only if the triple describes a WordNet synset of a Wikipedia category.
wikicatOfWordNetT :: Either a YagoRdfTriple -> Maybe Text
wikicatOfWordNetT (Right (_,ts@(YagoWikicat cat),tv,to@(YagoWordnet synset)) ) = x
  where x = Just (T.concat [cat, "\t",synset])
wikicatOfWordNetT _ = Nothing

-- | returns only if the triple is an interlink between (English) Wikipedia.
interWikiLinks :: Either a YagoRdfTriple -> Maybe Text
interWikiLinks (Right (_,ts@(YagoWikiTitle s),_,to@(YagoWikiTitle o))) = Just (T.intercalate "\t" [s,o])
interWikiLinks _ = Nothing

wordnetType :: Either a YagoRdfTriple -> Maybe Text
wordnetType (Right (_,ts@(YagoWikiTitle s),_,to@(YagoWordnet o))) = Just (T.intercalate "\t" [s,o])
wordnetType _ = Nothing

wordnetTaxonomy :: Either a YagoRdfTriple -> Maybe Text
wordnetTaxonomy (Right (_,ts@(YagoWordnet s),YagoRDFSprop p,to@(YagoWordnet o))) |p == "subClassOf"= Just (T.intercalate "\t" [s,o])
wordnetTaxonomy _ = Nothing

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

newtype Opt = Opt Text
            deriving (Show, Eq)

selector :: Opt -> Text -> Maybe Text
selector opt | opt == Opt "typedCat"  = wikicatOfWordNetT.readlineYAGO  -- for generating the `typedCats` file
selector opt | opt == Opt "interlink" = interWikiLinks . readlineYAGO   -- for generating the `interlinks` file
selector opt | opt == Opt "synset"    = wordnetType . readlineYAGO
selector opt | opt == Opt "taxonomy"  = wordnetTaxonomy . readlineYAGO
selector _opt                         = error "Unknown option"

{-|
  This is for filtering specific RDF triples and print them wiht a custom representation.
-}
yago :: (Text -> Maybe Text) -> Text -> Text -> IO Text
yago f prevPartialBlock block = do
  let
    (mainBlock,partialBlock) = T.breakOnEnd "\n" block
    ls = T.lines (T.append prevPartialBlock mainBlock)
    filtered = mapMaybe f ls
  mapM_ T.IO.putStrLn filtered
  return partialBlock

main :: IO ()
main = do
  args <- getArgs
  let
    opt = Opt (T.pack (head args))
  readBlocks stdin (yago (selector opt)) ""
