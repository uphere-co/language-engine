{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.RDF.Yago where

import           Data.Text                             (Text)
import           Data.Binary                           (Binary)
import           GHC.Generics                          (Generic)
import qualified Data.ByteString.Lazy.Char8      as BL


data YagoObject = YagoID        Text
                | YagoRDFverb   Text
                | YagoOWLclass  Text
                | YagoRDFSprop  Text
                | YagoSKOSverb  Text
                | YagoVerb      Text
                | YagoNoun      Text
                | YagoWordnet   Text
                | YagoWikicat   Text
                | YagoClass     Text
                | YagoWikiTitle Text
                | YagoWikiAlias Text
                | YagoNonEnWikiTitle Text Text -- lang title
                | YagoNonEnWikiAlias Text Text -- lang alias
                | YagoTypedValue Text Text     -- type value
                | YagoTextValue Text
                | YagoURL       Text
                deriving (Eq, Show, Generic)
instance Binary YagoObject

type YagoRdfTriple = (YagoObject, YagoObject, YagoObject, YagoObject)

