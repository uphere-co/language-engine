{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import           Data.Attoparsec.Text
import           Control.Applicative                   ((<|>))

{-
de/Carl_Albrecht_Bernoulli>    <hasWikipediaAnchorText>    "نيتشه"@ara .
<Hezbollah>    <hasWikipediaAnchorText>    "الحدود اللبنانية الإسرائيلية"@ara   


# yagoLabels
<Bitter_&_Sweet_Release_Tour_Final>    rdfs:label    "Bitter & Sweet Release Tour Final"@eng .
<Fred_Severud>    <redirectedFrom>    "Fred N. Severud"@eng .
<de/Hermann_Krahforst>    <hasFamilyName>    "Krahforst"@de .
<nl/Tibirke_(parochie)>    skos:prefLabel    "Tibirke (parochie)"@eng .


yagoDateFacts
<Guus_Kouwenhoven>    <wasBornOnDate>    "1942-02-15"^^xsd:date .
<Watsonalla_uncinula>    <wasCreatedOnDate>    "1790-##-##"^^xsd:date .

<Saccobolus_glaber>    rdf:type    <wordnet_fungus_112992868> .
<Salmonella_Dub>    rdf:type    <yagoPermanentlyLocatedEntity> .
<Salmonella_Dub>    rdf:type    <wordnet_social_group_107950920> .
<Jean-Baptiste-Joseph_Gobel>    rdf:type    <wikicat_French_politicians> .
<Jean-Baptiste-Joseph_Gobel>    rdf:type    <wordnet_alumnus_109786338> .
<Saccobolus_glaber>    rdf:type    owl:Thing .

<de/Preußische_ES_51_bis_ES_57>

# yagoTypes.tsv : types of entities
<id_1kmo9y9_88c_1e08w1o>    <PowerVR>    rdf:type    <wikicat_Companies_based_in_Hertfordshire>   
<id_1kmo9y9_88c_1g5nyun>    <PowerVR>    rdf:type    <wikicat_Computer_hardware_companies>   
<id_1kmo9y9_88c_1eoxwov>    <PowerVR>    rdf:type    <wikicat_Graphics_hardware_companies>   
<id_13tyf46_88c_4gx1l8>    <de/NEC_PowerVR_PCX>    rdf:type    <wikicat_Graphics_chips>

newtype YagoID = YagoID Text
               deriving (Eq, Ord)

instance Show YagoID where
  show (YagoID uid) = "YagoID:" ++ show uid
-}

data YagoObject = YagoID        Text
                | YagoRDFverb   Text
                | YagoOWLclass  Text
                | YagoRDFSprop  Text
                | YagoWordnet   Text
                | YagoWikicat   Text
                | YagoClass     Text
                | YagoWikiTitle Text
                | YagoNonEnWiki Text
                deriving (Eq, Show)



object = takeTill (\x -> x=='>' || C.isSpace x)
ssep = skipWhile C.isSpace

parserYAGOtoken :: Text -> Text -> (Text -> a) -> Parser a
parserYAGOtoken prefix postfix f = do
  string prefix
  t <- object
  string postfix
  return (f t)


parserYAGOuid, parserRDFverb, parserOWLclass, parserRDFSprop:: Parser YagoObject
parserYAGOuid  = parserYAGOtoken "<id_" ">" YagoID
parserRDFverb  = parserYAGOtoken "rdf:" ""  YagoRDFverb
parserOWLclass = parserYAGOtoken "owl:" ""  YagoOWLclass
parserRDFSprop = parserYAGOtoken "rdfs:" "" YagoRDFSprop


parserYAGOwordnet, parserYAGOwikicat, parserYAGOclass :: Parser YagoObject
parserYAGOwordnet = parserYAGOtoken "<wordnet_" ">" YagoWordnet
parserYAGOwikicat = parserYAGOtoken "<wikicat_" ">" YagoWikicat
parserYAGOclass   = parserYAGOtoken "<yago" ">"     YagoClass

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
  return (YagoNonEnWiki t)



parserNounToken, parserVerbToken, parserUIDToken :: Parser YagoObject
parserNounToken = choice [ parserYAGOwordnet
                         , parserYAGOwikicat
                         , parserYAGOwikiTitle
                         , parserYAGOnonEnwikiTitle
                         , parserYAGOclass]

parserVerbToken = choice [ parserRDFverb
                         , parserOWLclass
                         , parserRDFSprop
                         , parserYAGOnonEnwikiTitle]

parserUIDToken  = parserYAGOuid


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
  eassertEqual (parseOnly parserVerbToken "owl:Thing") (Right (YagoOWLclass "Thing"))
  eassertEqual (parseOnly parserVerbToken "rdfs:subClassOf")        (Right (YagoRDFSprop "subClassOf"))
  eassertEqual (parseOnly parserUIDToken  "<id_5kujp2_1m6_a52wvp>") (Right (YagoID "5kujp2_1m6_a52wvp"))
  eassertEqual (parseOnly parserNounToken "<wordnet_organization_108008335>")      (Right (YagoWordnet "organization_108008335"))
  eassertEqual (parseOnly parserNounToken "<wikicat_Graphics_hardware_companies>") (Right (YagoWikicat "Graphics_hardware_companies"))
  eassertEqual (parseOnly parserNounToken "<PowerVR>")            (Right (YagoWikiTitle "PowerVR"))
  eassertEqual (parseOnly parserNounToken "<de/NEC_PowerVR_PCX>") (Right (YagoNonEnWiki "NEC_PowerVR_PCX"))
  eassertEqual (parseOnly parserNounToken "<yagoPermanentlyLocatedEntity>") (Right (YagoClass "PermanentlyLocatedEntity"))

testYagoTaxonomyTSVrows :: TestTree
testYagoTaxonomyTSVrows = testCaseSteps "Parse lines in YAGO dump for taxonomy, yagoTaxonomy.tsv" $ \step -> do
 
  let 
    lines = ["<id_4gx1l8_1m6_1snupo6> <wikicat_Graphics_chips> rdfs:subClassOf <wordnet_bit_109222051>"
            ,"<id_k664kn_1m6_130ah1o>    <wordnet_company_108058098>    rdfs:subClassOf    <wordnet_institution_108053576>"
            ,"<id_130ah1o_1m6_5kujp2>    <wordnet_institution_108053576>    rdfs:subClassOf    <wordnet_organization_108008335>"
            ,"<id_5kujp2_1m6_1qdi4lo>    <wordnet_organization_108008335>    rdfs:subClassOf    <yagoLegalActor>"
            ,"<id_5kujp2_1m6_a52wvp>    <wordnet_organization_108008335>    rdfs:subClassOf    <wordnet_social_group_107950920> "
            ,"<id_1kmo9y9_88c_1eoxwov>    <PowerVR>    rdf:type    <wikicat_Graphics_hardware_companies>   "
            ,"<id_13tyf46_88c_4gx1l8>\t\t<de/NEC_PowerVR_PCX>    rdf:type    <wikicat_Graphics_chips>    "
            ]
    expected=[Right (YagoID "4gx1l8_1m6_1snupo6", YagoWikicat "Graphics_chips",   YagoRDFSprop "subClassOf", YagoWordnet "bit_109222051")
             ,Right (YagoID "k664kn_1m6_130ah1o", YagoWordnet "company_108058098",YagoRDFSprop "subClassOf", YagoWordnet "institution_108053576")
             ,Right (YagoID "130ah1o_1m6_5kujp2", YagoWordnet "institution_108053576", YagoRDFSprop "subClassOf", YagoWordnet "organization_108008335")
             ,Right (YagoID "5kujp2_1m6_1qdi4lo", YagoWordnet "organization_108008335",YagoRDFSprop "subClassOf", YagoClass"LegalActor")
             ,Right (YagoID "5kujp2_1m6_a52wvp",  YagoWordnet "organization_108008335",YagoRDFSprop "subClassOf", YagoWordnet "social_group_107950920")
             ,Right (YagoID "1kmo9y9_88c_1eoxwov",YagoWikiTitle "PowerVR", YagoRDFverb "type", YagoWikicat "Graphics_hardware_companies")
             ,Right (YagoID "13tyf46_88c_4gx1l8", YagoNonEnWiki "NEC_PowerVR_PCX", YagoRDFverb "type", YagoWikicat "Graphics_chips")
             ]
    rows = map (parseOnly parserRDFrowInTSV) lines 
  mapM_ (uncurry eassertEqual) (zip rows expected)

allTest :: TestTree
allTest =
  testGroup
    "All Unit tests"
    [testYagoRdfObjects, testYagoTaxonomyTSVrows]    

