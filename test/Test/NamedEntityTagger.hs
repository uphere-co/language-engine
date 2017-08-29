{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.NamedEntityTagger where

import           Data.Text                             (Text)
import           Data.Vector                           (Vector,fromList,toList)
import           Control.Arrow                         (first,second)
import           Assert                                (assert,massertEqual,eassertEqual)
import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Vector                   as V

import           WikiEL.CoreNLP                               (parseNEROutputStr)
import           WikiEL.WikiEntityTagger                      (loadWETagger,wikiAnnotator)
import           WikiEL.WikiEntityClass                       (fromFiles,getNEClass)
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),entityLinking,entityLinkings,buildEntityMentions,entityUID)
import           WikiEL.ETL.LoadData
-- For testing:
import           WikiEL.Misc                                  (IRange(..),untilOverlapOrNo,untilNoOverlap,relativePos, isContain,subVector)
import qualified NLP.Type.NamedEntity          as N
import           NLP.Type.PennTreebankII    (POSTag(..))
import qualified WikiEL.WikiEntityClass        as WC
-- to be moved
import           Data.Text.Encoding                    (encodeUtf8)
import           Data.Digest.XXHash                    (XXHash,xxHash')
import qualified Data.Vector.Unboxed           as UV
import           Data.Map                              (Map)
import           Data.Maybe                            (mapMaybe)

import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.WordNet
import           WikiEL.Type.Equity
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser
import           WikiEL.WordNet

import qualified Data.Map                      as M
import qualified WikiEL.EntityLinking          as EL
import qualified WikiEL.Type.FileFormat        as F
import qualified WikiEL                        as WEL
import qualified WikiEL.EntityMentionPruning   as EMP

import           Test.Data.Filename

uid = itemID
uids = fromList . map uid


other wikiUID  = (uid wikiUID, WC.otherClass)
org   wikiUID  = (uid wikiUID, WC.orgClass)
person wikiUID = (uid wikiUID, WC.personClass)

ai1 = other "Q42970"
ai2 = other "Q11660"
nlp = other "Q30642"
google       = org "Q95"
googleSearch = other "Q9366"
facebook     = org "Q380"


testNamedEntityTagging :: TestTree
testNamedEntityTagging = testCaseSteps "Named entity tagging on CoreNLP NER output" $ \step -> do
  entities <- loadWETagger reprFileTiny
  let
    ner_text = "Google/ORGANIZATION and/O Facebook/ORGANIZATION Inc./ORGANIZATION are/O famous/O AI/O companies/O ./O NLP/ORGANIZATION stands/O for/O natural/O language/O processing/O ./O"
    stanford_nefs =  map parseStanfordNE (parseNEROutputStr ner_text)
    uid2tag = WC.fromList [nlp,ai1,ai2,facebook,google,googleSearch]
    matchedItems  = namedEntityAnnotator entities uid2tag stanford_nefs
    expected_matches = [(IRange 0 1,   fromList [google, googleSearch])
                       ,(IRange 2 4,   fromList [facebook])
                       ,(IRange 6 7,   fromList [ai2, ai1])
                       ,(IRange 9 10,  fromList [nlp])
                       ,(IRange 12 15, fromList [nlp])
                       ]
    tt = getStanfordNEs stanford_nefs
    expected_tt = [(IRange 0 1, N.Org),(IRange 2 4,N.Org),(IRange 9 10, N.Org)]

    -- Other : (uids ["Q11660","Q9366","Q30642"])
    -- Org  :  (uids ["Q42970","Q380","Q95"])    
    
  --print ner_text
  eassertEqual tt expected_tt
  eassertEqual matchedItems expected_matches

testIRangeOps :: TestTree
testIRangeOps = testCaseSteps "Test operations on IRange" $ \step -> do
  let
    ranges1 = [IRange 0 2, IRange 2 4, IRange 5 7, IRange 10 12]
    ranges2 = [IRange 0 2, IRange 2 4, IRange 7 8, IRange 10 12]
    ref = IRange 1 6
    dist_ref = relativePos ref
  eassertEqual (untilNoOverlap   dist_ref ranges1) [IRange 10 12]
  eassertEqual (untilNoOverlap   dist_ref ranges2) [IRange 7 8, IRange 10 12]
  eassertEqual (untilOverlapOrNo dist_ref ranges1) [IRange 5 7, IRange 10 12]
  eassertEqual (untilOverlapOrNo dist_ref ranges2) [IRange 7 8, IRange 10 12]


testNEResolution :: TestTree
testNEResolution = testCaseSteps "Resolving Wiki UID with Stanford NE tag" $ \step -> do
  let
    ambiguousUID = fromList [org "Q1", org "Q2", person "Q3"]
    entities = [(IRange 1 4, ambiguousUID)]
  
  eassertEqual (resolveNEClass N.Org ambiguousUID) (AmbiguousUID [uid "Q2", uid "Q1"])
  eassertEqual (resolveNEClass N.Person ambiguousUID) (Resolved (uid "Q3", N.Person))

  step "Single entity cases"
  eassertEqual (resolveNEs [(IRange 1 4, N.Person)] entities) [(IRange 1 4, resolveNEClass N.Person ambiguousUID)]
  eassertEqual (resolveNEs [(IRange 1 4, N.Org)] entities) [(IRange 1 4, resolveNEClass N.Org ambiguousUID)]
  eassertEqual (resolveNEs [(IRange 1 2, N.Org)] entities) [(IRange 1 4, UnresolvedClass (toList ambiguousUID))]
  eassertEqual (resolveNEs [(IRange 0 5, N.Org)] entities) [(IRange 0 5, UnresolvedUID N.Org)]
  eassertEqual (resolveNEs [(IRange 0 2, N.Org)] entities) [(IRange 0 2, UnresolvedUID N.Org)]
  eassertEqual (resolveNEs [(IRange 3 5, N.Org)] entities) [(IRange 1 4, UnresolvedClass (toList ambiguousUID))]

  step "Multiple entities cases"
  let
    input = "A1/PERSON A2/PERSON x/O y/O z/O W1/ORGANIZATION W2/ORGANIZATION W3/ORGANIZATION"
    stanford_nes =  getStanfordNEs (map parseStanfordNE (parseNEROutputStr input))
    ambiguousUID1 = fromList [org "Q11", org "Q12", person "Q13"]
    ambiguousUID2 = fromList [org "Q21", org "Q22", person "Q23"]
    entities1 = [(IRange 0 2, ambiguousUID1),(IRange 5 8, ambiguousUID2)]
    
    r1 = resolveNEs stanford_nes entities1
    expected_r1 = [(IRange 0 2, Resolved (uid "Q13", N.Person)),
                   (IRange 5 8, AmbiguousUID [uid "Q22", uid "Q21"])]

    entities2 = [(IRange 0 2, ambiguousUID1),(IRange 5 7, ambiguousUID2)]
    r2 = resolveNEs stanford_nes entities2
    expected_r2 = [(IRange 0 2, Resolved (uid "Q13", N.Person)),
                   (IRange 5 8, UnresolvedUID N.Org)]

    entities3 = [(IRange 0 2, ambiguousUID1),(IRange 4 6, ambiguousUID2)]
    r3 = resolveNEs stanford_nes entities3
    expected_r3 = [(IRange 0 2, Resolved (uid "Q13", N.Person)),
                   (IRange 4 6, UnresolvedClass (toList ambiguousUID2))]

    entities4 = [(IRange 0 2, ambiguousUID1),(IRange 7 9, ambiguousUID2)]
    r4 = resolveNEs stanford_nes entities4
    expected_r4 = expected_r2

  print stanford_nes
  eassertEqual r1 expected_r1
  --eassertEqual r2 expected_r2
  --eassertEqual r3 expected_r3
  --eassertEqual r4 expected_r4  

testWikiNER :: TestTree
testWikiNER = 
  testGroup
    "Unit tests for WikiNER"
      [testNamedEntityTagging, testNEResolution]


testEntityMentionProperties :: TestTree
testEntityMentionProperties = testCaseSteps "Test helper functions for accessing info on EntityMention" $ \step -> do
  let
    united_airlines = fromList ["United","Airlines"]
    emuid = EL.EntityMentionUID
    -- t1 :: EMInfo Text
    t1 = (IRange 0 2,   united_airlines, Resolved (uid "Q174769", N.Org)) 
    -- em :: EntityMention Text
    em = EL.Self (emuid 0) t1
  eassertEqual (WEL.mentionedEntityName em) "United Airlines"

testRunWikiNER :: TestTree
testRunWikiNER = testCaseSteps "Test run for Wiki named entity annotator" $ \step -> do
  input_raw <- T.IO.readFile rawNewsFile
  input <- T.IO.readFile nerNewsFile
  uid2tag <- fromFiles [(WC.orgClass, orgItemFile), (WC.personClass, personItemFile)]
  wikiTable <- loadWETagger reprFile
  
  let
    stanford_nefs = map parseStanfordNE (parseNEROutputStr input)
    named_entities =  filter (\x -> snd x == N.Org || snd x == N.Person) (getStanfordNEs stanford_nefs)
    wiki_entities = namedEntityAnnotator wikiTable uid2tag stanford_nefs
    wiki_named_entities = resolveNEs named_entities wiki_entities

    text = fromList (T.words input_raw)
    mentions = buildEntityMentions text wiki_named_entities
    linked_mentions = entityLinkings mentions
    
    -- constants for testing
    flag1 = getNEClass uid2tag (uid "Q95")
    flag2 = getNEClass uid2tag (uid "Q3503829")
    united_airlines = fromList ["United","Airlines"]
    oscar_munoz     = fromList ["Oscar","Munoz"]
    t1  = (IRange 0 2,   united_airlines, Resolved (uid "Q174769", N.Org))
    t1' = (IRange 90 92, united_airlines, Resolved (uid "Q174769", N.Org))
    t2  = (IRange 5 7,   oscar_munoz, Resolved (uid "Q21066734", N.Person))
    t2' = (IRange 95 97, oscar_munoz, Resolved (uid "Q21066734", N.Person))
    t3  = (IRange 10 11, fromList ["Munoz"], UnresolvedUID N.Person)
    t4  = (IRange 13 14, fromList ["United"], UnresolvedUID N.Org)

  --eassertEqual (entityLinking [] t4) (Self 0 t4)

  print "Named entities"
  {-
  mapM_ print named_entities
  print "Wiki entities"
  mapM_ print wiki_entities
  print "Wiki named entities"
  mapM_ print wiki_named_entities
  print "Entity mentions"
  mapM_ print mentions
  print "Entity-linked named entities"
  mapM_ print linked_mentions
  -}



data SubclassRelationFile = SubclassRelationFile FilePath
data PublicCompanyFile = PublicCompanyFile FilePath

parseFail :: Either String a -> Bool 
parseFail (Left  _) = True
parseFail (Right _) = False



testHelperUtils :: TestTree
testHelperUtils = testCaseSteps "Test for helper functions on general algorithms" $ \step -> do
  assert $ isContain (fromList [1,2]) (fromList [1,2,3])
  assert $ not $ isContain (fromList [1,2]) (fromList [1])
  assert $ isContain (fromList [1,2]) (fromList [0,1,2,3])


testParsingSubclassRelation :: TestTree
testParsingSubclassRelation = testCaseSteps "Test for parsing Wikidata P31 subclass_of data files" $ \step -> do
  eassertEqual (itemID "Q131") (ItemID 131)
  assert $ parseFail (parseItemID "P31")
  assert $ parseFail (parseItemID "QQ11")
  let testLine = "Q5119\tcapital\tQ515\tcity"
  T.IO.putStrLn testLine
  eassertEqual (subclassRelation testLine) (SubclassRelationRow (itemID "Q5119") (itemID "Q515"))
  print $ subclassRelation testLine
    
testParsingPublicCompanyInfo :: TestTree 
testParsingPublicCompanyInfo = testCaseSteps "Test for parsing listed company info" $ \step -> do
  let
    testLine = "Abiomed\tABMD\tHealth Care\tHealth Care Equipment\t6872689\tQ4667884"
    expected = ("Abiomed", GICS "Health Care", GICSsub "Health Care Equipment", Symbol "ABMD", PageID 6872689, ItemID 4667884)
    testDataPath = PublicCompanyFile "enwiki/companies"
  eassertEqual (publicCompany testLine) expected
  print expected

testParsingData :: TestTree  
testParsingData =
  testGroup
    "Tests for loading data files"
    [testParsingSubclassRelation, testParsingPublicCompanyInfo]    


testParsingWordNetSynsetFile :: TestTree
testParsingWordNetSynsetFile = testCaseSteps "Test for mapping between Wikidata and WordNet" $ \step -> do
  let
    testcase1 = "synset-incumbent-noun-1"
    testcase2 = "synset-dirty-case_like-this-one-noun-2"
    r1 = wordnetSynset testcase1
    r2 = wordnetSynset testcase2
  eassertEqual r1 (Synset "incumbent" "noun" 1)
  eassertEqual r2 (Synset "dirty-case_like-this-one" "noun" 2)


testEntityMentionRefinerPOS :: TestTree
testEntityMentionRefinerPOS = testCaseSteps "Test for filtering entity mentions based on POS tags" $ \step -> do
  assert (EMP.isEntityLinkableTag NNP)
  assert ((not . EMP.isEntityLinkableTag) MD)
  assert ((not . EMP.isEntityLinkableTag) PRP)
  --assert (isEntityLinkableTag VBG)

  let
    texts = V.fromList [NNP,MD,MD, NNP,NNP]
  assert (EMP.isEntityLinkable texts (IRange 0 1))
  assert (EMP.isEntityLinkable texts (IRange 1 4))
  assert (not (EMP.isEntityLinkable texts (IRange 1 3)))

allTest :: TestTree
allTest =
  testGroup
    "All NamedEntityTagger unit tests"
    [ testHelperUtils
    , testIRangeOps
    , testWikiNER
    , testEntityMentionProperties
    , testRunWikiNER
    , testParsingData
    , testParsingWordNetSynsetFile
    , testEntityMentionRefinerPOS
    ]    




getOrgs :: EntityMention a -> Maybe (EntityMentionUID, ItemID)
getOrgs (EL.Self muid (_,_, Resolved (wuid, N.Org))) = Just (muid, wuid)
getOrgs (EL.Cite muid _ (_,_, Resolved (wuid, N.Org))) = Just (muid, wuid)
getOrgs _ = Nothing

getCompanySymbol :: Map ItemID Symbol -> (EntityMentionUID, ItemID) -> Maybe (EntityMentionUID , ItemID, Symbol)
getCompanySymbol tikcerMap (mentionUID, itemID) = result
  where
    result = case M.lookup itemID tikcerMap of
      Just symbol -> Just (mentionUID, itemID, symbol)
      Nothing     -> Nothing

runEL (tickerMap,uid2tag,wikiTable) rawFile nerFile posFile = do
  let
    -- Load data for entity mention pruner. Input is a list of PoS tags of the input text.
    input_pos = V.fromList (map fst posFile)
  input_raw <- T.IO.readFile rawFile
  input_ner <- T.IO.readFile nerFile
  let
    stanford_nefs  = map parseStanfordNE (parseNEROutputStr input_ner)
    named_entities = getStanfordNEs stanford_nefs -- filter (\x -> snd x == N.Org || snd x == N.Person) 
    wiki_entities  = namedEntityAnnotator wikiTable uid2tag stanford_nefs
    wiki_named_entities = resolveNEs named_entities wiki_entities

    text = fromList (T.words input_raw)
    mentions = buildEntityMentions text wiki_named_entities
    all_linked_mentions = entityLinkings mentions

    -- Pruning (linked) entity mentions. 
    linked_mentions = EMP.filterEMbyPOS input_pos all_linked_mentions
    -- Company ticker symbol lookup.
    orgMentions = mapMaybe getOrgs linked_mentions
    companyWithSymbols = mapMaybe (getCompanySymbol tickerMap) orgMentions

  -- For loading WordNet synsets data.
  --wordNetMapping <- loadWordNetMapping wordnetMappingFile
  let
    -- WordNet synset lookup.
    --wn = buildWordNetSynsetLookup wordNetMapping
    --synsets = map (lookupWordNet wn . entityUID) linked_mentions
  mapM_ print named_entities
  mapM_ print wiki_entities
  print "Entity-linked named entities"
  -- mapM_ print all_linked_mentions  
  mapM_ print (filter EL.hasResolvedUID linked_mentions)
  --mapM_ print synsets -- takes many minutes
  print "Entity-linked organization entities"
  mapM_ print orgMentions
  print "Entity-linked public company entities"
  mapM_ print companyWithSymbols

main1 = do
  -- For loading ticker symbol data.
  tickerMap <- loadCompanySymbol listedCompanyFile
  uid2tag <- fromFiles [ (WC.personClass, personItemFile)
                       , (WC.orgClass, orgItemFile)
                       , (WC.brandClass, brandItemFile)
                       , (WC.occupationClass, occupationItemFile)
                       , (WC.locationClass, locationItemFile)
                       , (WC.humanRuleClass, humanRuleItemFile)
                       , (WC.buildingClass, buildingItemFile)
                       ]
  wikiTable <- loadWETagger reprFile


  runEL (tickerMap,uid2tag,wikiTable) rawNewsFile3 nerNewsFile3 posNewsFile3
  runEL (tickerMap,uid2tag,wikiTable) rawNewsFile4 nerNewsFile4 posNewsFile4
  runEL (tickerMap,uid2tag,wikiTable) rawNewsFile5 nerNewsFile5 posNewsFile5


  
main2 = do
    propertyNames  <- loadPropertyNames  propertyNameFile
    wordNetMapping <- loadWordNetMapping wordnetMappingFile
    let
      lookupWordNet key = M.lookup key (buildWordNetSynsetLookup wordNetMapping)
      
    mapM_ print (Prelude.take 50 propertyNames)
    mapM_ print (Prelude.take 50 wordNetMapping)
    eassertEqual (lookupWordNet (itemID "Q218217")) (Just [Synset "incumbent" "noun" 1, Synset "congressman" "noun" 1])

