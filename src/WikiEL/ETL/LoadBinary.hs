module WikiEL.ETL.LoadBinary where

import qualified Data.Binary                     as B
import qualified Data.ByteString.Lazy.Char8      as BL

import           WikiEL.Type.RDF.Wikidata
import           WikiEL.Type.RDF.Yago


decodeYagoRDF :: BL.ByteString  -> [YagoRdfTriple]
decodeYagoRDF = B.decode

encode :: B.Binary a => a -> BL.ByteString
encode = B.encode

