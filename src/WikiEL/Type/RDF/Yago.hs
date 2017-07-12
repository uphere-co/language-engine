{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.RDF.Yago where

import           Data.Text                             (Text)

data YagoObject = YagoID        Text
                | YagoRDFverb   Text
                | YagoOWLclass  Text
                | YagoRDFSprop  Text
                | YagoSKOSverb  Text
                | YagoVerb      Text
                | YagoWordnet   Text
                | YagoWikicat   Text
                | YagoClass     Text
                | YagoWikiTitle Text
                | YagoWikiAlias Text
                | YagoNonEnWikiTitle Text Text
                | YagoNonEnWikiAlias Text Text
                deriving (Eq, Show)

type YagoRdfTriple = (YagoObject, YagoObject, YagoObject, YagoObject)

