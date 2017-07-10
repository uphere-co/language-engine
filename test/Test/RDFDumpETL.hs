{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.RDFDumpETL where

import           Data.Maybe                            (fromMaybe)
import           Data.Text                             (Text)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Char                     as C
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.WikiEntityClass                (SuperclassUID(..),SubclassUID(..),fromRows,buildRelations,allRelationPairs,getAncestors,isSubclass)
import           WikiEL.Misc                           (IRange(..))
import           Assert                                (assert,massertEqual,eassertEqual)
import           WikiEL.WikiEntityTagger


import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser
import           WikiEL.ETL.LoadData

--For testing
import           Text.RawString.QQ

import           Data.Attoparsec.Text
import           Control.Applicative                   ((<|>))

{-
yagoDateFacts
<Guus_Kouwenhoven>    <wasBornOnDate>    "1942-02-15"^^xsd:date .
<Watsonalla_uncinula>    <wasCreatedOnDate>    "1790-##-##"^^xsd:date .

newtype YagoID = YagoID Text
               deriving (Eq, Ord)

instance Show YagoID where
  show (YagoID uid) = "YagoID:" ++ show uid
-}

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
                | YagoNonEnWikiTitle Text
                | YagoNonEnWikiAlias Text
                deriving (Eq, Show)

type YagoRdfTriple = (YagoObject, YagoObject, YagoObject, YagoObject)


object = takeTill (\x -> x=='>' || C.isSpace x)
ssep = skipWhile C.isSpace

parserYAGOtoken :: Text -> Text -> (Text -> a) -> Parser a
parserYAGOtoken prefix postfix f = do
  string prefix
  t <- object
  string postfix
  return (f t)


parserYAGOuid, parserRDFverb, parserRDFSprop,parserSKOSverb,parserYAGOverb:: Parser YagoObject
parserYAGOuid  = parserYAGOtoken "<id_" ">" YagoID
parserRDFverb  = parserYAGOtoken "rdf:" ""  YagoRDFverb
parserRDFSprop = parserYAGOtoken "rdfs:" "" YagoRDFSprop
parserSKOSverb = parserYAGOtoken "skos:" "" YagoSKOSverb
parserYAGOverb = parserYAGOtoken "<" ">"    YagoVerb


parserYAGOwordnet, parserYAGOwikicat, parserOWLclass, parserYAGOclass :: Parser YagoObject
parserYAGOwordnet = parserYAGOtoken "<wordnet_" ">" YagoWordnet
parserYAGOwikicat = parserYAGOtoken "<wikicat_" ">" YagoWikicat
parserOWLclass    = parserYAGOtoken "owl:" ""       YagoOWLclass
parserYAGOclass   = parserYAGOtoken "<yago" ">"     YagoClass

parserYAGOwikiAlias :: Parser YagoObject
parserYAGOwikiAlias = do
  let alias = takeTill (=='"')
  string "\""
  t <- alias
  string "\"@eng"
  return (YagoWikiAlias t)


parserYAGOwikiTitle :: Parser YagoObject
parserYAGOwikiTitle = do
  string "<"
  fst <- satisfy (not . C.isLower)
  rest <- object
  string ">"
  let title = T.cons fst rest
  return (YagoWikiTitle title)

--Title by international wikis, except English one.
parserYAGOnonEnwikiTitle :: Parser YagoObject
parserYAGOnonEnwikiTitle = do
  string "<"
  c1 <- satisfy C.isLower
  c2 <- satisfy C.isLower
  string "/"
  t <- object
  string ">"
  return (YagoNonEnWikiTitle t)



parserNounToken, parserVerbToken, parserUIDToken :: Parser YagoObject
parserNounToken = choice [ parserYAGOwordnet
                         , parserYAGOwikicat
                         , parserOWLclass
                         , parserYAGOclass
                         , parserYAGOwikiAlias
                         , parserYAGOwikiTitle
                         , parserYAGOnonEnwikiTitle]
parserVerbToken = choice [ parserRDFverb
                         , parserRDFSprop
                         , parserSKOSverb
                         , parserYAGOverb]
parserUIDToken  = parserYAGOuid

parserRDFrowInTSV :: Parser YagoRdfTriple
parserRDFrowInTSV = do
  uid  <- parserUIDToken
  ssep
  subj <- parserNounToken
  ssep
  verb <- parserVerbToken
  ssep
  obj  <- parserNounToken
  ssep
  return (uid, subj, verb, obj)

testYagoRdfObjects :: TestTree
testYagoRdfObjects = testCaseSteps "YAGO objects in RDF dumps." $ \step -> do
  eassertEqual (parseOnly parserVerbToken "rdf:type")  (Right (YagoRDFverb "type"))
  eassertEqual (parseOnly parserVerbToken "rdfs:subClassOf")        (Right (YagoRDFSprop "subClassOf"))
  eassertEqual (parseOnly parserVerbToken "skos:prefLabel")         (Right (YagoSKOSverb "prefLabel"))
  eassertEqual (parseOnly parserVerbToken "<hasWikipediaAnchorText>") (Right (YagoVerb "hasWikipediaAnchorText"))
  eassertEqual (parseOnly parserVerbToken "<redirectedFrom>")         (Right (YagoVerb "redirectedFrom"))
  eassertEqual (parseOnly parserUIDToken  "<id_5kujp2_1m6_a52wvp>") (Right (YagoID "5kujp2_1m6_a52wvp"))
  eassertEqual (parseOnly parserNounToken "owl:Thing") (Right (YagoOWLclass "Thing"))
  eassertEqual (parseOnly parserNounToken "<wordnet_organization_108008335>")      (Right (YagoWordnet "organization_108008335"))
  eassertEqual (parseOnly parserNounToken "<wikicat_Graphics_hardware_companies>") (Right (YagoWikicat "Graphics_hardware_companies"))
  eassertEqual (parseOnly parserNounToken "<PowerVR>")            (Right (YagoWikiTitle "PowerVR"))
  eassertEqual (parseOnly parserNounToken "<de/NEC_PowerVR_PCX>") (Right (YagoNonEnWikiTitle "NEC_PowerVR_PCX"))
  eassertEqual (parseOnly parserNounToken "<yagoPermanentlyLocatedEntity>") (Right (YagoClass "PermanentlyLocatedEntity"))
  eassertEqual (parseOnly parserNounToken "\"Demography of Afghanistan\"@eng") (Right (YagoWikiAlias "Demography of Afghanistan"))

testYagoTaxonomyTSVrows :: TestTree
testYagoTaxonomyTSVrows = testCaseSteps "Parse lines in YAGO dump for taxonomy, yagoTaxonomy.tsv" $ \step -> do
  let 
    lines = [-- samples from : yagoTaxonomy.tsv
             "<id_4gx1l8_1m6_1snupo6>\t<wikicat_Graphics_chips> rdfs:subClassOf <wordnet_bit_109222051>"
            ,"<id_k664kn_1m6_130ah1o>    <wordnet_company_108058098>    rdfs:subClassOf    <wordnet_institution_108053576>"
            ,"<id_130ah1o_1m6_5kujp2>\t<wordnet_institution_108053576>    rdfs:subClassOf    <wordnet_organization_108008335>"
            ,"<id_5kujp2_1m6_1qdi4lo>\t<wordnet_organization_108008335>    rdfs:subClassOf    <yagoLegalActor>"
            ,"<id_5kujp2_1m6_a52wvp> <wordnet_organization_108008335>    rdfs:subClassOf    <wordnet_social_group_107950920>"
            ,"<id_klokc9_1m6_1koas7s>\t<wikicat_Syntactic_entities>\trdfs:subClassOf\towl:Thing  "
            -- samples from : yagoTypes.tsv
            ,"<id_1kmo9y9_88c_1eoxwov>    <PowerVR>    rdf:type    <wikicat_Graphics_hardware_companies>   "
            ,"<id_13tyf46_88c_4gx1l8>\t<de/NEC_PowerVR_PCX>    rdf:type    <wikicat_Graphics_chips>    "
            -- samples from yagoLabels.tsv
            ,"<id_we31u1_1sz_vfg0ga>\t<Burnside,_Iowa>\tskos:prefLabel\t\"Burnside, Iowa\"@eng"
            ,"<id_1qt4wt3_1ia_1k16t4w>\t<pl/Olli_Tyrväinen>\trdfs:label \"Olli Tyrvainen\"@eng    "
            ,"<id_1j3k64j_qkd_4hgw54> <Fred_M._Hechinger>\t<redirectedFrom> \"Fred Hechinger\"@eng "
            -- samples from yagoConteXtFacts_en.tsv : No ID for RDF triples
            --"	<Afghani_man>	<hasWikipediaAnchorText>	\"Demography of Afghanistan\"@eng	"
            --"	<AssistiveTechnology>	<hasWikipediaAnchorText>	\"Assistive_technology\"@eng"
            ]
    expected=[Right (YagoID "4gx1l8_1m6_1snupo6", YagoWikicat "Graphics_chips",   YagoRDFSprop "subClassOf", YagoWordnet "bit_109222051")
             ,Right (YagoID "k664kn_1m6_130ah1o", YagoWordnet "company_108058098",YagoRDFSprop "subClassOf", YagoWordnet "institution_108053576")
             ,Right (YagoID "130ah1o_1m6_5kujp2", YagoWordnet "institution_108053576", YagoRDFSprop "subClassOf", YagoWordnet "organization_108008335")
             ,Right (YagoID "5kujp2_1m6_1qdi4lo", YagoWordnet "organization_108008335",YagoRDFSprop "subClassOf", YagoClass"LegalActor")
             ,Right (YagoID "5kujp2_1m6_a52wvp",  YagoWordnet "organization_108008335",YagoRDFSprop "subClassOf", YagoWordnet "social_group_107950920")
             ,Right (YagoID "klokc9_1m6_1koas7s", YagoWikicat "Syntactic_entities", YagoRDFSprop "subClassOf", YagoOWLclass "Thing")
             
             ,Right (YagoID "1kmo9y9_88c_1eoxwov",YagoWikiTitle "PowerVR", YagoRDFverb "type", YagoWikicat "Graphics_hardware_companies")
             ,Right (YagoID "13tyf46_88c_4gx1l8", YagoNonEnWikiTitle "NEC_PowerVR_PCX", YagoRDFverb "type", YagoWikicat "Graphics_chips")
             
             ,Right (YagoID "we31u1_1sz_vfg0ga", YagoWikiTitle "Burnside,_Iowa", YagoSKOSverb "prefLabel", YagoWikiAlias "Burnside, Iowa")
             ,Right (YagoID "1qt4wt3_1ia_1k16t4w",YagoNonEnWikiTitle "Olli_Tyrväinen", YagoRDFSprop "label", YagoWikiAlias "Olli Tyrvainen")
             ,Right (YagoID "1j3k64j_qkd_4hgw54", YagoWikiTitle "Fred_M._Hechinger", YagoVerb "redirectedFrom", YagoWikiAlias "Fred Hechinger")
             ]
    rows = map (parseOnly parserRDFrowInTSV) lines 
  mapM_ (uncurry eassertEqual) (zip rows expected)

allYagoTest :: TestTree
allYagoTest =
  testGroup
    "All YAGO Unit tests"
    [testYagoRdfObjects, testYagoTaxonomyTSVrows]    



data TurtleState = Comma 
                 | Semicolon
                 | End
                 deriving (Show, Eq)

data TurtleRelation = RelationSVO Text Text Text 
                    | RelationVO  Text Text
                    | RelationO   Text
                    deriving (Show)
--parserWikidataRdfEndOfLine 

parserWikiAlias :: Parser Text
parserWikiAlias = do
  let alias = takeTill (=='"')
  string "\""
  t <- alias
  string "\"@eng"
  return t

parserNonEnWikiAlias :: Parser Text
parserNonEnWikiAlias = do
  let alias = takeTill (=='"')
  string "\""
  t <- alias
  string "\"@"
  lan <- takeWhile1 C.isLower
  return (T.concat [t, "@",lan])

wikidataObject = parserWikiAlias <|> parserNonEnWikiAlias <|> takeWhile1 (not . C.isSpace)
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

parserWikidataRdfRelation = choice [ parserWikidataRdfRelation3
                                   , parserWikidataRdfRelation2
                                   , parserWikidataRdfRelation1]
                          
splitTripleWithState :: Text -> (Text, TurtleState)
splitTripleWithState line = (T.strip row, nextState)
  where
    input = T.strip line
    f ',' = Comma
    f ';' = Semicolon
    f '.' = End
    f _   = error "Unknown line end."
    row = T.init input
    nextState = f (T.last input)

testWikidataRDFdumpTTL :: TestTree
testWikidataRDFdumpTTL = testCaseSteps "Parse a full RDF dump of Wikidata in Turtle format(.ttl)" $ \step -> do
  let
    case1 = T.pack ([r|wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672 a wikibase:Statement,
		wikibase:BestRank ;
	wikibase:rank wikibase:NormalRank ;
	ps:P414 wd:Q2632892 ;
	pq:P249 "AVAZ" ;
	prov:wasDerivedFrom wdref:2d11114e74636670e7d7b2ee58260de401e31e95 .|])
    lines1 = map splitTripleWithState (T.lines case1)
    rs1    =  map (\(x,y) -> parseOnly parserWikidataRdfRelation x) lines1

  mapM_ print rs1
  mapM_ print lines1
  print $ splitTripleWithState "a b c .\n"
  print $ splitTripleWithState "a b c,\n"
  print $ splitTripleWithState "  d ;\n"
  print $ splitTripleWithState "  b d,\n"
  print $ splitTripleWithState "    c,\n"
  print $ splitTripleWithState "    c .\n"
  let 
    (row, nextState) = splitTripleWithState "    c.\n"
  print row
  print $ parseOnly parserWikidataRdfRelation row
  print $ parseOnly parserWikidataRdfRelation "a b \"c d e\"@eng"
  print $ parseOnly parserWikidataRdfRelation "a b \"c d e\"@ru"
  print $ parseOnly parserWikidataRdfRelation "b c"

allWikidataTest :: TestTree
allWikidataTest =
  testGroup
    "All Wikidata Unit tests"
    [testWikidataRDFdumpTTL]    



allTest :: TestTree
allTest =
  testGroup
    "All Unit tests"
    [allYagoTest, allWikidataTest]    

