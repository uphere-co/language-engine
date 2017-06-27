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
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,buildTagUIDTable,getStanfordNEs,parseStanfordNE,namedEntityAnnotator)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..),resolveNEClass)
import           WikiEL.EntityLinking                         (EntityMentionUID,EntityMention(..),entityLinking,entityLinkings,buildEntityMentions)
import           WikiEL.ETL.LoadData

-- For testing:
import           WikiEL.Misc                                  (IRange(..),untilOverlapOrNo,untilNoOverlap,relativePos, isContain,subVector)
import qualified NLP.Type.NamedEntity                 as N
import qualified WikiEL.WikiEntityClass               as WC

-- to be moved

import           Data.Map                              (Map)
import           Data.Maybe                            (mapMaybe)
import qualified Data.Map                      as M
import qualified WikiEL.EntityLinking          as EL
import           WikiEL.Type.Wikidata
import           WikiEL.Type.Wikipedia
import           WikiEL.Type.Equity
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.Parser

import           Test.Data.Filename

uid = itemID
uids = fromList . map uid


other wikiUID = (uid wikiUID, N.Other)
org wikiUID = (uid wikiUID, N.Org)
person wikiUID = (uid wikiUID, N.Person)

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
                       ,(IRange 6 7,   fromList [ai1, ai2])
                       ,(IRange 9 10,  fromList [nlp])
                       ,(IRange 12 15, fromList [nlp])
                       ]
    tt = getStanfordNEs stanford_nefs
    expected_tt = [(IRange 0 1, N.Org),(IRange 2 4,N.Org),(IRange 9 10, N.Org)]

    others = buildTagUIDTable N.Other (uids ["Q11660","Q9366","Q30642"])
    orgs   = buildTagUIDTable N.Org   (uids ["Q42970","Q380","Q95"])    
    --TODO: sort UID
    uidTags = mconcat [others, orgs]
    
  --print uidTags
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
    ambiguousUID = fromList [org"Q1", org "Q2", person "Q3"]
    entities = [(IRange 1 4, ambiguousUID)]
  eassertEqual (resolveNEClass N.Org ambiguousUID) (AmbiguousUID [uid "Q2", uid "Q1"])
  eassertEqual (resolveNEClass N.Person ambiguousUID) (Resolved (person "Q3"))

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
    expected_r1 = [(IRange 0 2, Resolved (person "Q13")),
                   (IRange 5 8, AmbiguousUID [uid "Q22", uid "Q21"])]

    entities2 = [(IRange 0 2, ambiguousUID1),(IRange 5 7, ambiguousUID2)]
    r2 = resolveNEs stanford_nes entities2
    expected_r2 = [(IRange 0 2, Resolved (person "Q13")),
                   (IRange 5 8, UnresolvedUID N.Org)]

    entities3 = [(IRange 0 2, ambiguousUID1),(IRange 4 6, ambiguousUID2)]
    r3 = resolveNEs stanford_nes entities3
    expected_r3 = [(IRange 0 2, Resolved (person "Q13")),
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


testRunWikiNER :: TestTree
testRunWikiNER = testCaseSteps "Test run for Wiki named entity annotator" $ \step -> do
  input_raw <- T.IO.readFile rawNewsFile
  input <- T.IO.readFile nerNewsFile
  uid2tag <- fromFiles [(N.Org, orgItemFile), (N.Person, personItemFile)]
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
    t1  = (IRange 0 2,   united_airlines, Resolved (org "Q174769"))
    t1' = (IRange 90 92, united_airlines, Resolved (org "Q174769"))
    t2  = (IRange 5 7,   oscar_munoz, Resolved (person "Q21066734"))
    t2' = (IRange 95 97, oscar_munoz, Resolved (person "Q21066734"))
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

allTest :: TestTree
allTest =
  testGroup
    "All NamedEntityTagger unit tests"
    [testHelperUtils, testIRangeOps, testWikiNER, testRunWikiNER, testParsingData]    




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

main1 = do
  file <- T.IO.readFile listedCompanyFile

  input_raw <- T.IO.readFile rawNewsFile
  input <- T.IO.readFile nerNewsFile
  uid2tag <- fromFiles [(N.Org, orgItemFile), (N.Person, personItemFile)]
  wikiTable <- loadWETagger reprFile

  let 
    lines = T.lines file
    companies = map publicCompany lines
    tickers  = map (\(_,_,_,symbol,_,itemID) -> (itemID, symbol)) companies
    tickerMap = M.fromList tickers

    stanford_nefs = map parseStanfordNE (parseNEROutputStr input)
    named_entities =  filter (\x -> snd x == N.Org || snd x == N.Person) (getStanfordNEs stanford_nefs)
    wiki_entities = namedEntityAnnotator wikiTable uid2tag stanford_nefs
    wiki_named_entities = resolveNEs named_entities wiki_entities

    text = fromList (T.words input_raw)
    mentions = buildEntityMentions text wiki_named_entities
    linked_mentions = entityLinkings mentions

    orgMentions = mapMaybe getOrgs linked_mentions
    companyWithSymbols = mapMaybe (getCompanySymbol tickerMap) orgMentions

  print "Entity-linked named entities"
  mapM_ print linked_mentions
  print "Entity-linked organization entities"
  mapM_ print orgMentions
  print "Entity-linked public company entities"
  mapM_ print companyWithSymbols

  --print tickerMap


main2 = do
    let 
      propertyFile = propertyNameFile
    propertyNames <- loadPropertyNames propertyFile

    mapM_ print propertyNames
    