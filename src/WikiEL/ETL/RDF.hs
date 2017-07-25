module WikiEL.ETL.RDF  
  ( module WikiEL.Type.RDF.Wikidata
  , module WikiEL.Type.RDF.Yago
  , readlineYAGO
  , readlineWikidata
  , WikiEL.ETL.RDF.Wikidata.flattenStatement
  , WikiEL.ETL.RDF.Wikidata.flattenStatementStream
  , WikiEL.ETL.RDF.Wikidata.initState
  , WikiEL.ETL.RDF.Wikidata.ParsingState
  ) where

import           Data.Attoparsec.Text                  (parseOnly)

import           WikiEL.Type.RDF.Wikidata
import           WikiEL.Type.RDF.Yago
import           WikiEL.ETL.RDF.Wikidata
import           WikiEL.ETL.RDF.Yago

readlineYAGO = parseOnly parserRDFrowInTSV

readlineWikidata = parseRDFline parserWikidataRdfRelation . splitTripleWithState
