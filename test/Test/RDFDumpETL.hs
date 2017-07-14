{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.RDFDumpETL where

import           Data.Either                           (Either(..),rights)
import           Control.Arrow                         (first,second)
import           Data.List                             (foldl')
import           Data.Text                             (Text)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           WikiEL.WikiEntityClass                (SuperclassUID(..),SubclassUID(..),fromRows,buildRelations,allRelationPairs,getAncestors,isSubclass)
import           WikiEL.Misc                           (IRange(..))
import           Assert                                (assert,massertEqual,eassertEqual)
import           WikiEL.WikiEntityTagger


import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.FileFormat
import           WikiEL.Type.RDF.Wikidata
import           WikiEL.Type.RDF.Yago
import           WikiEL.ETL.Parser
import           WikiEL.ETL.LoadData
import           WikiEL.ETL.RDF.Wikidata
import           WikiEL.ETL.RDF.Yago

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
  eassertEqual (parseOnly parserNounToken "<de/NEC_PowerVR_PCX>") (Right (YagoNonEnWikiTitle "de" "NEC_PowerVR_PCX"))
  eassertEqual (parseOnly parserNounToken "<yagoPermanentlyLocatedEntity>") (Right (YagoClass "PermanentlyLocatedEntity"))
  eassertEqual (parseOnly parserNounToken "\"Demography of Afghanistan\"@eng") (Right (YagoWikiAlias "Demography of Afghanistan"))
  eassertEqual (parseOnly parserNounToken "\"Demography of Afghanistan\"@de") (Right (YagoNonEnWikiAlias "de" "Demography of Afghanistan"))

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
             ,Right (YagoID "13tyf46_88c_4gx1l8", YagoNonEnWikiTitle "de" "NEC_PowerVR_PCX", YagoRDFverb "type", YagoWikicat "Graphics_chips")
             
             ,Right (YagoID "we31u1_1sz_vfg0ga", YagoWikiTitle "Burnside,_Iowa", YagoSKOSverb "prefLabel", YagoWikiAlias "Burnside, Iowa")
             ,Right (YagoID "1qt4wt3_1ia_1k16t4w",YagoNonEnWikiTitle "pl" "Olli_Tyrväinen", YagoRDFSprop "label", YagoWikiAlias "Olli Tyrvainen")
             ,Right (YagoID "1j3k64j_qkd_4hgw54", YagoWikiTitle "Fred_M._Hechinger", YagoVerb "redirectedFrom", YagoWikiAlias "Fred Hechinger")
             ]
    rows = map (parseOnly parserRDFrowInTSV) lines 
  mapM_ (uncurry eassertEqual) (zip rows expected)

allYagoTest :: TestTree
allYagoTest =
  testGroup
    "All YAGO Unit tests"
    [testYagoRdfObjects, testYagoTaxonomyTSVrows]    



toWikidataObject x = r
  where
    Right r = parseOnly wikidataObject x    

relSVO s v o = RelationSVO (wo s) (wo v) (wo o)
  where wo = toWikidataObject
relVO    v o = RelationVO  (wo v) (wo o) 
  where wo = toWikidataObject
relO       o = RelationO   (wo o)
  where wo = toWikidataObject

testWikidataTurtleRelation :: TestTree
testWikidataTurtleRelation = testCaseSteps "Test case for parsing individual lines of Turtle format files" $ \step -> do
  eassertEqual (Left "Blank line.")        (splitTripleWithState "\n")
  eassertEqual (Left "Wrong end of line.") (splitTripleWithState "a b c\n")
  eassertEqual (Right ("a b c",End))   (splitTripleWithState "a b c .\n")
  eassertEqual (Right ("a b c",Comma)) (splitTripleWithState "a b c,\n")
  eassertEqual (Right ("d",Semicolon)) (splitTripleWithState "  d ;\n")
  eassertEqual (Right ("b d",Comma)) (splitTripleWithState "  b d,\n")
  eassertEqual (Right ("c",Comma))   (splitTripleWithState "    c,\n")
  eassertEqual (Right ("c",End))     (splitTripleWithState "    c .\n")
  let 
    Right (row, nextState) = splitTripleWithState "    c.\n"
  eassertEqual (Right (relO "c"))      (parseOnly parserWikidataRdfRelation row)
  eassertEqual (Right (relVO "b" "c")) (parseOnly parserWikidataRdfRelation "b c")
  eassertEqual (Right (relSVO "a" "b" "\"c d e\"@en"))    (parseOnly parserWikidataRdfRelation "a b \"c d e\"@en")
  eassertEqual (Right (relSVO "a" "b" "\"c d e\"@ru")) (parseOnly parserWikidataRdfRelation "a b \"c d e\"@ru")

testWikidataTurtleFillMissingSVO :: TestTree
testWikidataTurtleFillMissingSVO = testCaseSteps "Test case to get complete RDF triples in Turtle format" $ \step -> do
  let wo = toWikidataObject
  eassertEqual (fillMissingSV (End, wo "", wo "") (relSVO "a" "b" "c",End)) ((End, wo "a", wo "b"),   relSVO "a" "b" "c")
  eassertEqual (fillMissingSV (Comma, wo "x", wo "y") (relO "c",Comma))     ((Comma, wo "x", wo "y"), relSVO "x" "y" "c")
  eassertEqual (fillMissingSV (Semicolon, wo "x", wo "y") (relVO "b" "c",Comma)) ((Comma, wo "x", wo "b"), relSVO "x" "b" "c")

testWikidataRDFdumpTTL :: TestTree
testWikidataRDFdumpTTL = testCaseSteps "Parse a full RDF dump of Wikidata in Turtle format(.ttl)" $ \step -> do
  let
    case1 = T.pack ([r|
wd:Q2309 p:P414 wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672 .

wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672 a wikibase:Statement,
		wikibase:BestRank ;
	wikibase:rank wikibase:NormalRank ;
	ps:P414 wd:Q2632892 ;
	pq:P249 "AVAZ" ;
	prov:wasDerivedFrom wdref:2d11114e74636670e7d7b2ee58260de401e31e95 .
    |])
    lines1 = map splitTripleWithState (T.lines case1)
    rs1    =  map (parseRDFline parserWikidataRdfRelation) lines1
    expected1 = [ (relSVO "wd:Q2309" "p:P414" "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672 .", End)
                , (relSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:Statement",Comma)
                , (relO "wikibase:BestRank",Semicolon)
                , (relVO "wikibase:rank" "wikibase:NormalRank",Semicolon)
                , (relVO "ps:P414" "wd:Q2632892",Semicolon)
                , (relVO "pq:P249" "\"AVAZ\"",Semicolon)
                , (relVO "prov:wasDerivedFrom" "wdref:2d11114e74636670e7d7b2ee58260de401e31e95",End)
                ]
    triples1 = [ relSVO "wd:Q2309" "p:P414" "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672 ."
               , relSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:Statement"
               , relSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:BestRank"
               , relSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "wikibase:rank" "wikibase:NormalRank"
               , relSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "ps:P414" "wd:Q2632892"
               , relSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "pq:P249" "\"AVAZ\""
               , relSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "prov:wasDerivedFrom" "wdref:2d11114e74636670e7d7b2ee58260de401e31e95"
               ]
  mapM_ print lines1
  mapM_ (uncurry eassertEqual) (zip (rights rs1) expected1)
  mapM_ (uncurry eassertEqual) (zip triples1 (flattenStatement (rights rs1)))

  let
    case2 = T.pack ([r|wd:Q31 a wikibase:Item ;
        rdfs:label "Belgium"@en ;
        skos:prefLabel "Belgium"@en ;
        schema:name "Belgium"@en ;
        schema:description "constitutional monarchy in Western Europe"@en,
                "État d'Europe occidentale"@fr,
                "西欧国家"@zh-sg,
                "Staat an Europa"@lb ;
        skos:altLabel "Kingdom of Belgium"@en,
                "be"@en,
                "Королівство Бельгія"@uk ;
        wdt:P1464 wd:Q7463296 ;
        wdt:P1036 "2--493" ;
        wdt:P138 wd:Q206443 ;
        wdt:P31 wd:Q3624078,
                wd:Q43702,
                wd:Q160016,
                wd:Q6505795 ;
        wdt:P30 wd:Q46 ;
        wdt:P36 wd:Q239 ;
        wdt:P41 <http://commons.wikimedia.org/wiki/Special:FilePath/Flag%20of%20Belgium%20%28civil%29.svg> ;
        wdt:P297 "BE" ;
        wdt:P2853 wd:Q1378312,
                wd:Q2335536 ;
        wdt:P2927 "+0.8"^^xsd:decimal ;
        wdt:P1332 "Point(4.77 51.5)"^^geo:wktLiteral ;
        wdt:P3221 "destination/belgium" ;
        p:P1464 wds:Q31-b8a6b97e-4815-1e46-db4c-6b5807933064 .|])
    lines2 = map splitTripleWithState (T.lines case2)
    rs2    = map (parseRDFline parserWikidataRdfRelation) lines2
    triples2  = [ relSVO "wd:Q31" "a" "wikibase:Item"
                , relSVO "wd:Q31" "rdfs:label" "\"Belgium\"@en"
                , relSVO "wd:Q31" "skos:prefLabel" "\"Belgium\"@en"
                , relSVO "wd:Q31" "schema:name" "\"Belgium\"@en"
                , relSVO "wd:Q31" "schema:description" "\"constitutional monarchy in Western Europe\"@en"
                , relSVO "wd:Q31" "schema:description" "\"État d'Europe occidentale\"@fr"
                , relSVO "wd:Q31" "schema:description" "\"西欧国家\"@zh-sg"
                , relSVO "wd:Q31" "schema:description" "\"Staat an Europa\"@lb"
                , relSVO "wd:Q31" "skos:altLabel" "\"Kingdom of Belgium\"@en"
                , relSVO "wd:Q31" "skos:altLabel" "\"be\"@en"
                , relSVO "wd:Q31" "skos:altLabel" "\"Королівство Бельгія\"@uk"
                , relSVO "wd:Q31" "wdt:P1464" "wd:Q7463296"
                , relSVO "wd:Q31" "wdt:P1036" "\"2--493\""
                , relSVO "wd:Q31" "wdt:P138" "wd:Q206443"
                , relSVO "wd:Q31" "wdt:P31" "wd:Q3624078"
                , relSVO "wd:Q31" "wdt:P31" "wd:Q43702"
                , relSVO "wd:Q31" "wdt:P31" "wd:Q160016"
                , relSVO "wd:Q31" "wdt:P31" "wd:Q6505795"
                , relSVO "wd:Q31" "wdt:P30" "wd:Q46"
                , relSVO "wd:Q31" "wdt:P36" "wd:Q239"
                , relSVO "wd:Q31" "wdt:P41" "<http://commons.wikimedia.org/wiki/Special:FilePath/Flag%20of%20Belgium%20%28civil%29.svg>"
                , relSVO "wd:Q31" "wdt:P297" "\"BE\""
                , relSVO "wd:Q31" "wdt:P2853" "wd:Q1378312"
                , relSVO "wd:Q31" "wdt:P2853" "wd:Q2335536"
                , relSVO "wd:Q31" "wdt:P2927" "\"+0.8\"^^xsd:decimal"
                , relSVO "wd:Q31" "wdt:P1332" "\"Point(4.77 51.5)\"^^geo:wktLiteral"
                , relSVO "wd:Q31" "wdt:P3221" "\"destination/belgium\""
                , relSVO "wd:Q31" "p:P1464" "wds:Q31-b8a6b97e-4815-1e46-db4c-6b5807933064"
                ]
  mapM_ (uncurry eassertEqual) (zip triples2 (flattenStatement (rights rs2)))
  let
    lines = map splitTripleWithState (T.lines ( T.concat [case1,"\n\n", case2, "\n", case1]))
    rs    = map (parseRDFline parserWikidataRdfRelation) lines
  mapM_ print (flattenStatement (rights rs))

allWikidataTest :: TestTree
allWikidataTest =
  testGroup
    "All Wikidata Unit tests"
    [ testWikidataTurtleRelation
    , testWikidataTurtleFillMissingSVO
    , testWikidataRDFdumpTTL
    ]



allTest :: TestTree
allTest =
  testGroup
    "All Unit tests"
    [allYagoTest, allWikidataTest]    

