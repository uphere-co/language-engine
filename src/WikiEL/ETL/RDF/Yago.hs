{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.RDF.Yago where

import qualified Data.Text                     as T
import qualified Data.Char                     as C
import           Data.Attoparsec.Text

import           WikiEL.ETL.RDF.Common      (parserObject,parserObject2,parserTypedValue)
import           WikiEL.Type.RDF.Yago



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

parserYAGOtypedValue :: Parser YagoObject
parserYAGOtypedValue = parserTypedValue f
  where
    f text typeTag = YagoTypedValue typeTag text


parserNounToken, parserVerbToken, parserUIDToken :: Parser YagoObject
parserNounToken = choice [ parserYAGOwordnet
                         , parserYAGOwikicat
                         , parserOWLclass
                         , parserYAGOclass
                         , parserYAGOwikiAlias
                         , parserYAGOwikiTitle
                         , parserYAGOnonEnWikiAlias
                         , parserYAGOnonEnwikiTitle]
parserVerbToken = choice [ parserRDFverb
                         , parserRDFSprop
                         , parserSKOSverb
                         , parserYAGOverb]
parserUIDToken  = parserYAGOuid

parserRDFrowInTSV :: Parser YagoRdfTriple
parserRDFrowInTSV = do
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

