module WikiEL.ETL.RDF  
  ( module WikiEL.Type.RDF.Wikidata
  , module WikiEL.Type.RDF.Yago
  , readlineYAGO
  , readlineWikidata
  , WikiEL.ETL.RDF.Wikidata.flattenStatement
  ) where

import           Data.Attoparsec.Text                  (parseOnly)

import           WikiEL.Type.RDF.Wikidata
import           WikiEL.Type.RDF.Yago
import           WikiEL.ETL.RDF.Wikidata
import           WikiEL.ETL.RDF.Yago

readlineYAGO = parseOnly parserRDFrowInTSV

readlineWikidata = parseRDFline parserWikidataRdfRelation . splitTripleWithState
