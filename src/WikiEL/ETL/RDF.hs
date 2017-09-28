module WikiEL.ETL.RDF  
  ( module WikiEL.Type.RDF.Wikidata
  , module WikiEL.Type.RDF.Yago
  , readlineYAGO
  , readlineWikidata
  , mapParsed
  , WikiEL.ETL.RDF.Wikidata.flattenStatement
  , WikiEL.ETL.RDF.Wikidata.flattenStatementStream
  , WikiEL.ETL.RDF.Wikidata.initState
  , WikiEL.ETL.RDF.Wikidata.ParsingState
  ) where

import           Data.Text                             (Text)
import           Data.Attoparsec.Text                  (parseOnly)
import qualified Data.Either                     as E

import           WikiEL.Type.RDF.Wikidata
import           WikiEL.Type.RDF.Yago
import           WikiEL.ETL.RDF.Wikidata
import           WikiEL.ETL.RDF.Yago

{-|
  Each line of YAGO is a triple or quadruple. readlineYAGO can parse both format.
  It takes a line and may output a YagoRdfTriple if no error.
-}
readlineYAGO :: Text -> Either String YagoRdfTriple
readlineYAGO = parseOnly parserRDFrowInTSV

{-|
  It is for reading TTL format of Wikidata dump. 
  Note that each line of TTL format cannot be parsed individually. 
  To parse and output per line, readlineWikidata carries a state.
-}
readlineWikidata :: Text -> Either String (TurtleRelation, TurtleState)
readlineWikidata = parseRDFline parserWikidataRdfRelation . splitTripleWithState

mapParsed parser rows = E.rights (map parser rows)
