{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.RDF.Yago where

import qualified Data.Text                     as T
import qualified Data.Char                     as C
import           Data.Attoparsec.Text

import           WikiEL.ETL.RDF.Common      (parserObject,parserObject2,parserTypedValue)
import           WikiEL.Type.RDF.Yago


{-
  This is a module for parsing YAGO dumps.
  It is rather low level. In most client usages, it will be enough to check WikiEL.ETL.RDF.
-}

nullID :: YagoObject
nullID = YagoID "None"

parserYAGOuid, parserRDFverb, parserRDFSprop,parserSKOSverb,parserYAGOverb:: Parser YagoObject
parserYAGOuid  = parserObject "<id_" ">" YagoID
parserRDFverb  = parserObject "rdf:" ""  YagoRDFverb
parserRDFSprop = parserObject "rdfs:" "" YagoRDFSprop
parserSKOSverb = parserObject "skos:" "" YagoSKOSverb
parserYAGOverb = parserObject "<" ">"    YagoVerb


parserYAGOwordnet, parserYAGOwikicat, parserOWLclass, parserYAGOclass :: Parser YagoObject
parserYAGOwordnet = parserObject "<wordnet_" ">" YagoWordnet
parserYAGOwikicat = parserObject "<wikicat_" ">" YagoWikicat
parserOWLclass    = parserObject "owl:" ""       YagoOWLclass
parserYAGOclass   = parserObject "<yago" ">"     YagoClass

parserYAGOwikiAlias :: Parser YagoObject
parserYAGOwikiAlias = parserObject "\"" "\"@eng" YagoWikiAlias

parserYAGOnonEnWikiAlias :: Parser YagoObject
parserYAGOnonEnWikiAlias = parserObject2 "\"" "\"@" "" f
  where
    f alias country = YagoNonEnWikiAlias country alias

parserYAGOwikiTitle :: Parser YagoObject
parserYAGOwikiTitle = do
  char '<'
  fst <- satisfy (not . C.isLower)
  rest <- takeWhile1 (/='>')
  char '>'
  let title = T.cons fst rest
  return (YagoWikiTitle title)

--Title by international wikis, except English one.
--TODO: Didn't check the country code format.
parserYAGOnonEnwikiTitle :: Parser YagoObject
parserYAGOnonEnwikiTitle = parserObject2 "<" "/" ">" YagoNonEnWikiTitle

parserYAGOtypedValue, parserYAGOtextValue,parserYAGOurl :: Parser YagoObject
parserYAGOtypedValue = parserTypedValue f
  where
    f text typeTag = YagoTypedValue typeTag text
parserYAGOtextValue = parserObject "\"" "\""    YagoTextValue
parserYAGOurl   = parserObject "<http" ">"  toURL
  where toURL x = YagoURL (T.append "http" x)

parserNounToken, parserVerbToken, parserUIDToken,parserYAGOnoun :: Parser YagoObject
parserNounToken = choice [ -- Ordering matters. 
                           parserYAGOwordnet
                         , parserYAGOwikicat
                         , parserOWLclass
                         , parserYAGOclass
                         , parserYAGOwikiAlias
                         , parserYAGOwikiTitle
                         , parserYAGOnonEnWikiAlias
                         , parserYAGOtypedValue
                         , parserYAGOtextValue
                         , parserYAGOurl  -- Must be prior to parserYAGOnonEnwikiTitle.
                         , parserYAGOuid  -- Must be prior to parserYAGOnonEnwikiTitle.
                         , parserYAGOnonEnwikiTitle
                         , parserYAGOnoun -- Must be subsequent to parserYAGOnonEnwikiTitle.
                         ]
parserVerbToken = choice [ parserRDFverb
                         , parserRDFSprop
                         , parserSKOSverb
                         , parserYAGOverb
                         , parserOWLclass]
parserUIDToken  = parserYAGOuid
parserYAGOnoun = parserObject "<" ">"    YagoNoun

parserRDF4colRowInTSV :: Parser YagoRdfTriple
parserRDF4colRowInTSV = do
  let ssep = skipWhile C.isSpace
  uid  <- parserUIDToken
  ssep
  subj <- parserNounToken
  ssep
  verb <- parserVerbToken
  ssep
  obj  <- parserNounToken
  ssep
  return (uid, subj, verb, obj)

parserRDF3colRowInTSV :: Parser YagoRdfTriple
parserRDF3colRowInTSV = do
  let ssep = skipWhile C.isSpace
  ssep
  subj <- parserNounToken
  ssep
  verb <- parserVerbToken
  ssep
  obj  <- parserNounToken
  ssep
  return (nullID, subj, verb, obj)

parserRDFrowInTSV = choice [ parserRDF4colRowInTSV
                           , parserRDF3colRowInTSV]

