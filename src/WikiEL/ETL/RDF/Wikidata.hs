{-# LANGUAGE OverloadedStrings #-}

module WikiEL.ETL.RDF.Wikidata where

import           Data.Text                  (Text)
import           Data.Tuple                 (snd)
import           Data.List                  (foldl')
import           Data.Either                (Either)
import           Data.Attoparsec.Text
import qualified Data.Text                     as T
import qualified Data.Char                     as C

import           WikiEL.ETL.RDF.Common      (parserObject,parserObject2,parserTypedValue)
import           WikiEL.Type.RDF.Wikidata

{-
  This is a module for parsing Wikidata dumps.
  It is rather low level. In most client usages, it will be enough to check WikiEL.ETL.RDF.
-}

parserWikiAlias,parserWikiTypedValue,parserWikiTextValue,parserURLObject :: Parser WikidataObject
parserWikiAlias = parserObject2 "\"" "\"@" "" f
  where 
    f alias country | country == "en" = Alias alias
    f alias country                   = NonEnAlias country alias

parserWikiTypedValue = parserTypedValue f
  where
    f text typeTag = TypedValue typeTag text
parserWikiTextValue = parserObject "\"" "\"" TextValue
parserURLObject = parserObject "<" ">" URLObject

parserWikiNamedSpaceObject, parserWikiUnknownObject :: Parser WikidataObject
parserWikiNamedSpaceObject = do
  t <- takeWhile1 (\x -> (x/=':') && (not . C.isSpace) x)
  char ':'
  n <- takeWhile1 (not . C.isSpace)
  return (NameSpaceObject t n)

parserWikiUnknownObject = parserObject "" "" UnknownObject

wikidataObject :: Parser WikidataObject
wikidataObject = choice [ parserWikiAlias
                        , parserWikiTypedValue
                        , parserWikiTextValue
                        , parserURLObject
                        , parserWikiNamedSpaceObject
                        , parserWikiUnknownObject
                        ]

wikidataSep    = skipWhile C.isSpace 
--wikidataSep =  skipMany1 (skip C.isSpace)


parserWikidataRdfRelation3 :: Parser TurtleRelation
parserWikidataRdfRelation3 = do
  s <- wikidataObject
  wikidataSep
  v <- wikidataObject
  wikidataSep
  o <- wikidataObject
  return (RelationSVO s v o)

parserWikidataRdfRelation2 :: Parser TurtleRelation
parserWikidataRdfRelation2 = do
  v <- wikidataObject
  wikidataSep
  o <- wikidataObject
  return (RelationVO v o)

parserWikidataRdfRelation1 :: Parser TurtleRelation
parserWikidataRdfRelation1 = do
  o <- wikidataObject
  return (RelationO o)

parserWikidataRdfRelation :: Parser TurtleRelation
parserWikidataRdfRelation = choice [ parserWikidataRdfRelation3
                                   , parserWikidataRdfRelation2
                                   , parserWikidataRdfRelation1]


splitTripleWithState :: Text -> Either String (Text, TurtleState)
splitTripleWithState line = g row nextState
  where
    input = T.strip line
    row   = (T.strip . T.init) input
    f ',' = Right Comma
    f ';' = Right Semicolon
    f '.' = Right End
    f _   = Left "Wrong end of line."
    nextState = case input of
      "" -> Left "Blank line."
      _  -> f (T.last input)
    g row (Right state) = Right (row, state)
    g _ (Left msg) = Left msg

parseRDFline :: Parser TurtleRelation -> Either String (Text, TurtleState) -> Either String (TurtleRelation, TurtleState)
parseRDFline parser (Left msg) = Left msg
parseRDFline parser (Right (line, state)) = 
  case parseOnly parser line of
    (Right rel) -> Right (rel, state)
    (Left msg)  -> Left msg


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "Got something Left."

type ParsingState = (TurtleState, WikidataObject, WikidataObject)
initState :: ParsingState
initState = (End, dummy,dummy)
  where dummy = fromRight (parseOnly wikidataObject "_DUMMY_")

fillMissingSV :: ParsingState -> (TurtleRelation,TurtleState) -> (ParsingState, Either String TurtleRelation)
fillMissingSV (End, _,_)   (RelationSVO s' v' o', state') = ((state', s',v'), Right (RelationSVO s' v' o'))
fillMissingSV (Semicolon, s,v) (RelationVO v' o', state') = ((state', s, v'), Right (RelationSVO s v' o'))
fillMissingSV (Comma, s,v)     (RelationO  o',    state') = ((state', s, v),  Right (RelationSVO s v o'))
fillMissingSV x _ = (initState, Left ("Errors in filling missing SVO : " ++ show x))

flattenStatementStream :: ParsingState -> [Either String (TurtleRelation,TurtleState)] -> (ParsingState, [Either String TurtleRelation])
flattenStatementStream prevState rs = (lastState, reverse triples)
  where
    (lastState, triples) = foldl' f (prevState, []) rs
    f (state, ts) (Left msg)       = (state, Left msg : ts)
    f (state, ts) (Right relation) = (state', t:ts)
      where
        (state', t) = fillMissingSV state relation

flattenStatement :: [Either String (TurtleRelation,TurtleState)] -> [Either String TurtleRelation]
flattenStatement  = snd . flattenStatementStream initState
