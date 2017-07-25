{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.RDFDumpETL where

import           Data.Either                           (Either(..),rights)
import           Control.Arrow                         (first,second)
import           Data.Text                             (Text)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO

import           Assert                                (assert,massertEqual,eassertEqual)


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


-- 
--import qualified Data.Vector.Generic.Mutable   as M
--import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Unboxed           as UV
--import           Data.Vector.Generic.Mutable           (MVector)
--import           Data.Vector.Generic                   (Vector)
--import           Data.Vector.Unboxed           
import           Data.Digest.XXHash                    (XXHash,xxHash')
import           Data.Word                             (Word64)
import           Data.Int                              (Int32, Int64)
import           Data.Bits                             (shift)
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Binary                           (encode)
import           System.Random.MWC                     (createSystemRandom, withSystemRandom, asGenST, uniformVector)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           Data.Ord                              (Ordering,comparing)
import           Control.Lens.Getter                   ((^.))
import           Control.Lens.Setter                   ((.~))
import           Control.Lens.Tuple                    (_1,_2,_3,_4)
import           Control.Monad.ST                      (runST)


import           WikiEL.BinarySearch                   (binarySearchLR,binarySearchLRBy,binarySearchLRByBounds)


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
  eassertEqual (parseOnly parserUIDToken  "<id_5kujp2_1m6_a52wvp>")   (Right (YagoID "5kujp2_1m6_a52wvp"))
  eassertEqual (parseOnly parserNounToken "<id_5kujp2_1m6_a52wvp>")   (Right (YagoID "5kujp2_1m6_a52wvp"))
  eassertEqual (parseOnly parserNounToken "owl:Thing") (Right (YagoOWLclass "Thing"))
  eassertEqual (parseOnly parserNounToken "<wordnet_organization_108008335>")      (Right (YagoWordnet "organization_108008335"))
  eassertEqual (parseOnly parserNounToken "<wikicat_Graphics_hardware_companies>") (Right (YagoWikicat "Graphics_hardware_companies"))
  eassertEqual (parseOnly parserNounToken "<de/NEC_PowerVR_PCX>") (Right (YagoNonEnWikiTitle "de" "NEC_PowerVR_PCX"))
  eassertEqual (parseOnly parserNounToken "<yagoPermanentlyLocatedEntity>")    (Right (YagoClass "PermanentlyLocatedEntity"))
  eassertEqual (parseOnly parserNounToken "\"Demography of Afghanistan\"@eng") (Right (YagoWikiAlias "Demography of Afghanistan"))
  eassertEqual (parseOnly parserNounToken "\"Demography of Afghanistan\"@de")  (Right (YagoNonEnWikiAlias "de" "Demography of Afghanistan"))
  eassertEqual (parseOnly parserNounToken "\"Fred Hechinger\"")  (Right (YagoTextValue "Fred Hechinger"))
  eassertEqual (parseOnly parserNounToken "<http://www.example.com/>")  (Right (YagoURL "http://www.example.com/"))
  eassertEqual (parseOnly parserNounToken "<http://en.wikipedia.org/wiki/Donald_Trump%2C_Jr.>")  (Right (YagoURL "http://en.wikipedia.org/wiki/Donald_Trump%2C_Jr."))

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
            --"<hbfix3>\t<wordnet_cabbage_107713895>\trdfs:subClassOf\t<wordnet_cabbage_111875691>\t"
            -- samples from : yagoTypes.tsv
            ,"<id_1kmo9y9_88c_1eoxwov>    <PowerVR>    rdf:type    <wikicat_Graphics_hardware_companies>   "
            ,"<id_13tyf46_88c_4gx1l8>\t<de/NEC_PowerVR_PCX>    rdf:type    <wikicat_Graphics_chips>    "
            -- samples from yagoLabels.tsv
            ,"<id_we31u1_1sz_vfg0ga>\t<Burnside,_Iowa>\tskos:prefLabel\t\"Burnside, Iowa\"@eng"
            ,"<id_1qt4wt3_1ia_1k16t4w>\t<pl/Olli_Tyrväinen>\trdfs:label \"Olli Tyrvainen\"@eng    "
            ,"<id_1j3k64j_qkd_4hgw54> <Fred_M._Hechinger>\t<redirectedFrom> \"Fred Hechinger\"@eng "
            -- other samples from yago3_entire_tsv
            ,"        <fr/Alison_Moir>        <linksTo>       <Donald_Trump>"
            ,"        <Donald_Trump,_Jr.>     owl:sameAs      <http://dbpedia.org/resource/Donald_Trump,_Jr.>"
            ,"<id_1e1ybux_88c_1koas7s>        <Dubalgundi>    rdf:type        owl:Thing"
            ,"<id_1roo4zd_115_1uyvovk> <id_83enei_88c_zo71oj> <extractionSource> <http://en.wikipedia.org/wiki/Donald_Trump_%28song%29>"
            ,"<id_1gucu9u_p3m_zkjp59> <Donald_Trump,_Jr.>     <hasGender>     <male>"
            ]
    expected=[Right (YagoID "4gx1l8_1m6_1snupo6", YagoWikicat "Graphics_chips",   YagoRDFSprop "subClassOf", YagoWordnet "bit_109222051")
             ,Right (YagoID "k664kn_1m6_130ah1o", YagoWordnet "company_108058098",YagoRDFSprop "subClassOf", YagoWordnet "institution_108053576")
             ,Right (YagoID "130ah1o_1m6_5kujp2", YagoWordnet "institution_108053576", YagoRDFSprop "subClassOf", YagoWordnet "organization_108008335")
             ,Right (YagoID "5kujp2_1m6_1qdi4lo", YagoWordnet "organization_108008335",YagoRDFSprop "subClassOf", YagoClass "LegalActor")
             ,Right (YagoID "5kujp2_1m6_a52wvp",  YagoWordnet "organization_108008335",YagoRDFSprop "subClassOf", YagoWordnet "social_group_107950920")
             ,Right (YagoID "klokc9_1m6_1koas7s", YagoWikicat "Syntactic_entities", YagoRDFSprop "subClassOf", YagoOWLclass "Thing")
             
             ,Right (YagoID "1kmo9y9_88c_1eoxwov",YagoWikiTitle "PowerVR", YagoRDFverb "type", YagoWikicat "Graphics_hardware_companies")
             ,Right (YagoID "13tyf46_88c_4gx1l8", YagoNonEnWikiTitle "de" "NEC_PowerVR_PCX", YagoRDFverb "type", YagoWikicat "Graphics_chips")
             
             ,Right (YagoID "we31u1_1sz_vfg0ga",  YagoWikiTitle "Burnside,_Iowa", YagoSKOSverb "prefLabel", YagoWikiAlias "Burnside, Iowa")
             ,Right (YagoID "1qt4wt3_1ia_1k16t4w",YagoNonEnWikiTitle "pl" "Olli_Tyrväinen", YagoRDFSprop "label", YagoWikiAlias "Olli Tyrvainen")
             ,Right (YagoID "1j3k64j_qkd_4hgw54", YagoWikiTitle "Fred_M._Hechinger", YagoVerb "redirectedFrom", YagoWikiAlias "Fred Hechinger")
             
             ,Right (YagoID "None",YagoNonEnWikiTitle "fr" "Alison_Moir",YagoVerb "linksTo",YagoWikiTitle "Donald_Trump")
             ,Right (YagoID "None",YagoWikiTitle "Donald_Trump,_Jr.", YagoOWLclass "sameAs", YagoURL "http://dbpedia.org/resource/Donald_Trump,_Jr.")
             ,Right (YagoID "1e1ybux_88c_1koas7s", YagoWikiTitle "Dubalgundi", YagoRDFverb "type", YagoOWLclass "Thing")
             ,Right (YagoID "1roo4zd_115_1uyvovk",YagoID "83enei_88c_zo71oj",YagoVerb "extractionSource",YagoURL "http://en.wikipedia.org/wiki/Donald_Trump_%28song%29")
             ,Right (YagoID "1gucu9u_p3m_zkjp59",YagoWikiTitle "Donald_Trump,_Jr.",YagoVerb "hasGender",YagoNoun "male")
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

rightSVO s v o = Right (relSVO s v o)

testWikidataObject :: TestTree
testWikidataObject = testCaseSteps "Wikidata objects in RDF dumps." $ \step -> do
  eassertEqual (toWikidataObject "\"be\"@en")    (Alias "be")
  eassertEqual (toWikidataObject "\"be\"@en-gb") (NonEnAlias "en-gb" "be")
  eassertEqual (toWikidataObject "\"Maps of Scotland\"") (TextValue "Maps of Scotland")
  eassertEqual (toWikidataObject "wd:Q1") (NameSpaceObject "wd" "Q1")
  
testWikidataTurtleRelation :: TestTree
testWikidataTurtleRelation = testCaseSteps "Test case for parsing individual lines of Turtle format files" $ \step -> do
  eassertEqual (Left "Blank line.")        (splitTripleWithState "\n")
  eassertEqual (Left "Wrong end of line.") (splitTripleWithState "a b c\n")
  eassertEqual (Right ("a b c",End))   (splitTripleWithState "a b c .\n")
  eassertEqual (Right ("a b c",Comma)) (splitTripleWithState "a b c,\n")
  eassertEqual (Right ("d",Semicolon)) (splitTripleWithState "  d ;\n")
  eassertEqual (Right ("b d",  Comma)) (splitTripleWithState "  b d,\n")
  eassertEqual (Right ("c",    Comma)) (splitTripleWithState "    c,\n")
  eassertEqual (Right ("c",    End))   (splitTripleWithState "    c .\n")
  let 
    Right (row, nextState) = splitTripleWithState "    c.\n"
  eassertEqual (Right (relO "c"))      (parseOnly parserWikidataRdfRelation row)
  eassertEqual (Right (relVO "b" "c")) (parseOnly parserWikidataRdfRelation "b c")
  eassertEqual (Right (relSVO "a" "b" "\"c d e\"@en"))    (parseOnly parserWikidataRdfRelation "a b \"c d e\"@en")
  eassertEqual (Right (relSVO "a" "b" "\"c d e\"@ru")) (parseOnly parserWikidataRdfRelation "a b \"c d e\"@ru")

testWikidataTurtleFillMissingSVO :: TestTree
testWikidataTurtleFillMissingSVO = testCaseSteps "Test case to get complete RDF triples in Turtle format" $ \step -> do
  let wo = toWikidataObject
  eassertEqual (fillMissingSV (End, wo "", wo "") (relSVO "a" "b" "c",End)) ((End, wo "a", wo "b"),   Right (relSVO "a" "b" "c"))
  eassertEqual (fillMissingSV (Comma, wo "x", wo "y") (relO "c",Comma))     ((Comma, wo "x", wo "y"), Right (relSVO "x" "y" "c"))
  eassertEqual (fillMissingSV (Semicolon, wo "x", wo "y") (relVO "b" "c",Comma)) ((Comma, wo "x", wo "b"), Right (relSVO "x" "b" "c"))

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
    triples1 = [ Left "Blank line."
               , rightSVO "wd:Q2309" "p:P414" "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672 ."
               , Left "Blank line."
               , rightSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:Statement"
               , rightSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:BestRank"
               , rightSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "wikibase:rank" "wikibase:NormalRank"
               , rightSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "ps:P414" "wd:Q2632892"
               , rightSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "pq:P249" "\"AVAZ\""
               , rightSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "prov:wasDerivedFrom" "wdref:2d11114e74636670e7d7b2ee58260de401e31e95"
               , Left "Blank line."
               ]
  mapM_ print lines1
  mapM_ (uncurry eassertEqual) (zip (rights rs1) expected1)
  mapM_ (uncurry eassertEqual) (zip triples1 (flattenStatement rs1))

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
                "be"@en-gb,
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
    triples2  = [ rightSVO "wd:Q31" "a" "wikibase:Item"
                , rightSVO "wd:Q31" "rdfs:label" "\"Belgium\"@en"
                , rightSVO "wd:Q31" "skos:prefLabel" "\"Belgium\"@en"
                , rightSVO "wd:Q31" "schema:name" "\"Belgium\"@en"
                , rightSVO "wd:Q31" "schema:description" "\"constitutional monarchy in Western Europe\"@en"
                , rightSVO "wd:Q31" "schema:description" "\"État d'Europe occidentale\"@fr"
                , rightSVO "wd:Q31" "schema:description" "\"西欧国家\"@zh-sg"
                , rightSVO "wd:Q31" "schema:description" "\"Staat an Europa\"@lb"
                , rightSVO "wd:Q31" "skos:altLabel" "\"Kingdom of Belgium\"@en"
                , rightSVO "wd:Q31" "skos:altLabel" "\"be\"@en"
                , rightSVO "wd:Q31" "skos:altLabel" "\"be\"@en-gb"
                , rightSVO "wd:Q31" "skos:altLabel" "\"Королівство Бельгія\"@uk"
                , rightSVO "wd:Q31" "wdt:P1464" "wd:Q7463296"
                , rightSVO "wd:Q31" "wdt:P1036" "\"2--493\""
                , rightSVO "wd:Q31" "wdt:P138" "wd:Q206443"
                , rightSVO "wd:Q31" "wdt:P31" "wd:Q3624078"
                , rightSVO "wd:Q31" "wdt:P31" "wd:Q43702"
                , rightSVO "wd:Q31" "wdt:P31" "wd:Q160016"
                , rightSVO "wd:Q31" "wdt:P31" "wd:Q6505795"
                , rightSVO "wd:Q31" "wdt:P30" "wd:Q46"
                , rightSVO "wd:Q31" "wdt:P36" "wd:Q239"
                , rightSVO "wd:Q31" "wdt:P41" "<http://commons.wikimedia.org/wiki/Special:FilePath/Flag%20of%20Belgium%20%28civil%29.svg>"
                , rightSVO "wd:Q31" "wdt:P297" "\"BE\""
                , rightSVO "wd:Q31" "wdt:P2853" "wd:Q1378312"
                , rightSVO "wd:Q31" "wdt:P2853" "wd:Q2335536"
                , rightSVO "wd:Q31" "wdt:P2927" "\"+0.8\"^^xsd:decimal"
                , rightSVO "wd:Q31" "wdt:P1332" "\"Point(4.77 51.5)\"^^geo:wktLiteral"
                , rightSVO "wd:Q31" "wdt:P3221" "\"destination/belgium\""
                , rightSVO "wd:Q31" "p:P1464" "wds:Q31-b8a6b97e-4815-1e46-db4c-6b5807933064"
                ]
  let
    lines = map splitTripleWithState (T.lines ( T.concat [case1,"\n\n", case2, "\n", case1]))
    rs    = map (parseRDFline parserWikidataRdfRelation) lines
  --mapM_ print (flattenStatement (rights rs))
  mapM_ (uncurry eassertEqual) (zip triples2 (flattenStatement rs2))
  

allWikidataTest :: TestTree
allWikidataTest =
  testGroup
    "All Wikidata Unit tests"
    [ testWikidataTurtleRelation
    , testWikidataObject
    , testWikidataTurtleFillMissingSVO
    , testWikidataRDFdumpTTL
    ]




--newtype NewInt = NewInt Int
--               deriving (Eq,Ord,Show,UV.Unbox,MVector MVector, Vector Vector)
--newtype SomeID = SomeID Word64 deriving (Show,Eq,Unbox,M.MVector MVector,G.Vector Vector)
type UID  = Word64
type Subj = XXHash
type Verb = XXHash
type Obj  = XXHash
type Triple = (UID,Subj,Verb,Obj)

getUID  :: Triple -> UID
getUID  t = t^._1
getSubj :: Triple -> Subj
getSubj t = t^._2
getVerb :: Triple -> Verb
getVerb t = t^._3
getObj  :: Triple -> Obj
getObj  t = t^._4

-- XXHash is GHC.Word.Word32.
fromXXHash :: XXHash -> UID
fromXXHash = fromIntegral

fromXXHashPair :: XXHash -> XXHash -> UID
fromXXHashPair high low = fromXXHash low + shift (fromXXHash high) 32

fromText :: Text -> UID
fromText str = fromXXHashPair high low
  where
    hash = xxHash' . encodeUtf8
    (left,right) = T.splitAt (T.length str `div` 2) str
    (high, low)   = (hash left, hash right)


compareS,compareV,compareO :: Triple -> Triple -> Ordering
compareS lhs rhs = compare (getSubj lhs) (getSubj rhs)
compareV lhs rhs = compare (getVerb lhs) (getVerb rhs)
compareO lhs rhs = compare (getObj  lhs) (getObj  rhs)

compareSV,compareVS,compareVO,compareOV :: Triple -> Triple -> Ordering
compareSV lhs rhs | getSubj lhs == getSubj rhs  = compareV lhs rhs
                  | otherwise                   = compareS lhs rhs
compareVS lhs rhs | getVerb lhs == getVerb rhs  = compareS lhs rhs
                  | otherwise                   = compareV lhs rhs
compareVO lhs rhs | getVerb lhs == getVerb rhs  = compareO lhs rhs
                  | otherwise                   = compareV lhs rhs
compareOV lhs rhs | getObj lhs  == getObj rhs   = compareV lhs rhs
                  | otherwise                   = compareO lhs rhs

orderingBySVO,orderingByOVS :: Triple -> Triple -> Ordering
-- much slower version:
--orderingBySVO lhs rhs | getSubj lhs == getSubj rhs  = compareVO lhs rhs
--                      | otherwise                   = compareS  lhs rhs
--{- much faster version:
orderingBySVO lhs@(_,ls,lv,lo) rhs@(_,rs,rv,ro) = case compare ls rs of
  LT -> LT
  GT -> GT
  EQ -> case compare lv rv of
    LT -> LT
    GT -> GT
    EQ -> compare lo ro
---}
orderingByOVS lhs rhs | getObj  lhs == getObj  rhs  = compareVS lhs rhs
                      | otherwise                   = compareO  lhs rhs


randomTriple :: (XXHash,XXHash,XXHash,XXHash) -> Triple
randomTriple (high,s,v,o) = (fromXXHashPair high low, s `mod` nObject, v `mod` nProp, o `mod` nObject)
  where
    low = s+v+o -- adhoc hashing since Random.MWC only support up to 4 tuples.
    nObject = 100
    nProp   = 100

testInt64Hash :: TestTree
testInt64Hash = testCaseSteps "Tests for 64-bit hash" $ \step -> do
  eassertEqual (fromXXHashPair 1 1) 4294967297
  eassertEqual ((xxHash' . encodeUtf8) "") 46947589
  eassertEqual ((xxHash' . encodeUtf8) "a") 1426945110
  eassertEqual ((xxHash' . encodeUtf8) "bc") 2194405884
  eassertEqual (fromText "abc") (fromXXHashPair 1426945110 2194405884)
  eassertEqual (fromText   "a") (fromXXHashPair 46947589   1426945110) 
  eassertEqual (fromText    "") (fromXXHashPair 46947589   46947589)

garbageU = 0 :: UID
garbageS = 0 :: Subj
garbageV = 0 :: Verb
garbageO = 0 :: Obj

fillFromSV :: (Subj,Verb) -> Triple
fillFromSV (s,v) = (garbageU,s,v,garbageO)
fillFromVO :: (Verb,Obj)  -> Triple
fillFromVO (v,o) = (garbageU,garbageS,v,o)

lookupTriples comp conv vec val = runST $ do
  mvec <- UV.unsafeThaw vec
  (beg,end) <- binarySearchLRBy comp mvec (conv val)
  return (UV.slice beg (end-beg) vec)


testUnboxedVector :: TestTree
testUnboxedVector = testCaseSteps "Generate unboxed vector of random RDF triples" $ \step -> do
  print $ encode (211 :: Int)
  rs <- withSystemRandom . asGenST $ \gen -> uniformVector gen 20000
  let
    triples = UV.map randomTriple rs -- 1.2s with 0.2M triples
    triplesbySVO = UV.modify (sortBy orderingBySVO) triples -- 4.0s with 0.2M triples
    triplesbyOVS = UV.modify (sortBy orderingByOVS) triples
    lookupBySV = lookupTriples compareSV fillFromSV triplesbySVO
    lookupByOV = lookupTriples compareOV fillFromVO triplesbyOVS
  --mapM_ print (UV.toList (UV.slice 1000 50 triplesbySVO))
  --mapM_ print (UV.toList (UV.slice 1000 50 triplesbyOVS))

  step "list y for (x,3,y) where (1,2,x)"
  let
    v1in = [(1,2)] :: [(Subj,Verb)]
    v1out = UV.concat (map lookupBySV v1in)
    v2in  = UV.map    (\x -> (getObj x, 3:: Obj)) v1out
    v2out = UV.concat (map lookupBySV (UV.toList v2in))
  print "======================"
  print v1out
  print "----------------"
  print v2out
  print "/////////////////////////////////"
  
  step "list y for (x,3,y) where (x,1,2)"
  let
    w1in = [(1,2)] :: [(Verb,Obj)]
    w1out = UV.concat (map lookupByOV w1in)
    w2in  = UV.map    (\x -> (getSubj x, 3:: Verb)) w1out
    w2out = UV.concat (map lookupBySV (UV.toList w2in))
  print "======================"
  print w1out
  print "----------------"
  print w2out

allGenericRdfTripleTest :: TestTree
allGenericRdfTripleTest =
  testGroup
    "All unit tests for general RDF triple store"
    [ testInt64Hash
    , testUnboxedVector
    ]

allTest :: TestTree
allTest =
  testGroup
    "All Unit tests"
    [ allYagoTest
    , allWikidataTest
    , allGenericRdfTripleTest]    
