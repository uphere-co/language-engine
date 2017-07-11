{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.RDFDumpETL where

import           Data.Maybe                            (fromMaybe)
import           Control.Arrow                         (first,second)
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
                    deriving (Show, Eq)
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

{-
("wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672 a wikibase:Statement",Comma)
("wikibase:BestRank",Semicolon)
("wikibase:rank wikibase:NormalRank",Semicolon)
("ps:P414 wd:Q2632892",Semicolon)
("pq:P249 \"AVAZ\"",Semicolon)
("prov:wasDerivedFrom wdref:2d11114e74636670e7d7b2ee58260de401e31e95",End)

(Right (RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:Statement"),Comma)
(Right (RelationO "wikibase:BestRank"),Semicolon)
(Right (RelationVO "wikibase:rank" "wikibase:NormalRank"),Semicolon)
(Right (RelationVO "ps:P414" "wd:Q2632892"),Semicolon)
-}

fillMissingSV :: (TurtleState, Text, Text) -> (TurtleRelation,TurtleState) -> ((TurtleState, Text, Text), TurtleRelation)
fillMissingSV (End, _,_)   (RelationSVO s' v' o', state') = ((state', s',v'), RelationSVO s' v' o')
fillMissingSV (Semicolon, s,v) (RelationVO v' o', state') = ((state', s, v'), RelationSVO s v' o')
fillMissingSV (Comma, s,v)     (RelationO  o',    state') = ((state', s, v),  RelationSVO s v o')
fillMissingSV (_, _, _) _ = error "Wrong formats"

flattenStatementImpl :: (TurtleState,Text,Text) -> [TurtleRelation] -> [(TurtleRelation,TurtleState)] -> [TurtleRelation]
flattenStatementImpl _ ts []   = ts
flattenStatementImpl s ts (r:rs) = flattenStatementImpl s' (t:ts) rs
  where
    (s', t) = fillMissingSV s r

flattenStatement :: [(TurtleRelation,TurtleState)] -> [TurtleRelation]
flattenStatement rs = reverse (flattenStatementImpl (End,"","") [] rs)

{-
= foldl' f accum 
  where    
    f accum r = triple:accum
      where
        (state, triple) = fillMissingSV (End, "","") r

End -> take (Relation SVO, state)
(Semicolon S V) (RelationVO V' O', State) -> accum (S,V',O') and (State S V'),
(Comma S V) (RelatoinO O', State)  -> accum(S,V,O') and (State S V)
-}

testWikidataTurtleRelation :: TestTree
testWikidataTurtleRelation = testCaseSteps "Test case for parsing individual lines of Turtle format files" $ \step -> do
  eassertEqual ("a b c",End)   (splitTripleWithState "a b c .\n")
  eassertEqual ("a b c",Comma) (splitTripleWithState "a b c,\n")
  eassertEqual ("d",Semicolon) (splitTripleWithState "  d ;\n")
  eassertEqual ("b d",Comma) (splitTripleWithState "  b d,\n")
  eassertEqual ("c",Comma)   (splitTripleWithState "    c,\n")
  eassertEqual ("c",End)     (splitTripleWithState "    c .\n")
  let 
    (row, nextState) = splitTripleWithState "    c.\n"
  eassertEqual (Right (RelationO "c"))      (parseOnly parserWikidataRdfRelation row)
  eassertEqual (Right (RelationVO "b" "c")) (parseOnly parserWikidataRdfRelation "b c")
  eassertEqual (Right (RelationSVO "a" "b" "c d e"))    (parseOnly parserWikidataRdfRelation "a b \"c d e\"@eng")
  eassertEqual (Right (RelationSVO "a" "b" "c d e@ru")) (parseOnly parserWikidataRdfRelation "a b \"c d e\"@ru")

testWikidataTurtleFillMissingSVO :: TestTree
testWikidataTurtleFillMissingSVO = testCaseSteps "Test case to get complete RDF triples in Turtle format" $ \step -> do
  eassertEqual (fillMissingSV (End,"","") (RelationSVO "a" "b" "c",End)) ((End, "a", "b"),RelationSVO "a" "b" "c")
  eassertEqual (fillMissingSV (Comma,"x","y") (RelationO "c",Comma)) ((Comma, "x", "y"),RelationSVO "x" "y" "c")
  eassertEqual (fillMissingSV (Semicolon,"x","y") (RelationVO "b" "c",Comma)) ((Comma, "x", "b"),RelationSVO "x" "b" "c")

rightParse parser x = f r
  where
    r = parseOnly parser x
    f (Right r) = r
    f (Left  _) = error "Parse error"
    
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
    rs1    =  map (first (rightParse parserWikidataRdfRelation)) lines1
    expected1 = [(RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:Statement",Comma)
                ,(RelationO "wikibase:BestRank",Semicolon)
                ,(RelationVO "wikibase:rank" "wikibase:NormalRank",Semicolon)
                ,(RelationVO "ps:P414" "wd:Q2632892",Semicolon)
                ,(RelationVO "pq:P249" "\"AVAZ\"",Semicolon)
                ,(RelationVO "prov:wasDerivedFrom" "wdref:2d11114e74636670e7d7b2ee58260de401e31e95",End)
                ]
    triples1 = [RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:Statement"
               ,RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "a" "wikibase:BestRank"
               ,RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "wikibase:rank" "wikibase:NormalRank"
               ,RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "ps:P414" "wd:Q2632892"
               ,RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "pq:P249" "\"AVAZ\""
               ,RelationSVO "wds:Q2309-93C0587E-8BCE-4C97-835A-CF249E10C672" "prov:wasDerivedFrom" "wdref:2d11114e74636670e7d7b2ee58260de401e31e95"
               ]


  mapM_ (uncurry eassertEqual) (zip rs1 expected1)
  mapM_ (uncurry eassertEqual) (zip triples1 (flattenStatement rs1))

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

