{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Maybe                        (fromJust)
import           Data.List                         (inits)
import           Assert                            (assert,massertEqual,eassertEqual)
import           Test.Tasty.HUnit                  (testCase,testCaseSteps)
import           Test.Tasty                        (defaultMain, testGroup)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO

import qualified WikiEL.EntityLinking       as L
import           WikiEL.NamedEntity                (mergeToken)
import           NLP.Type.NamedEntity              (NamedEntity(..),parseStr)
import qualified WikiEL.CoreNLP             as C


parseStanfordNE (C.EntityToken (C.WordToken word) (C.NETag tag)) =  parseStr word tag

testNEParsing = testCaseSteps "Parsing text outputs of CoreNLP NER " $ \step -> do
-- Following rotines are for internal testing. User should not use these functions directly:
  eassertEqual (NamedEntityFrag "Oscar" Person)  (parseStanfordNE (C.parseNERToken "Oscar/PERSON"))
  eassertEqual (NamedEntity "Oscar Munoz" Person)  (fromJust (mergeToken (map parseStanfordNE (C.parseNEROutputStr "Oscar/PERSON Munoz/PERSON"))))
  eassertEqual (NamedEntity "United Airlines" Org) (fromJust (mergeToken [parseStr "United" "ORGANIZATION", parseStr "Airlines" "ORGANIZATION"]))
  eassertEqual (NamedEntityFrag "United Airlines" Org) (parseStr "United Airlines" "ORGANIZATION")


testContextedEntityLinkingImple = testCaseSteps "Modules for contexted entity linking " $ \step -> do
  let
    oscarMunoz = NamedEntity "Oscar Munoz" Person
    munoz      = NamedEntity "Munoz" Person
    munozOrg   = NamedEntity "Munoz" Org

    e0 = OrderedNamedEntity 0 oscarMunoz
    e1 = OrderedNamedEntity 1 munoz

    f0 = OrderedNamedEntity 0 munoz
    f1 = OrderedNamedEntity 1 oscarMunoz

  assert (L.mayRefer munoz oscarMunoz)
  assert (not (L.mayRefer munozOrg oscarMunoz))
  assert (L.canRefer e1 e0)
  assert (not (L.canRefer f0 f1))

testContextedEntityLinking = testCaseSteps "Contexted entity linking " $ \step -> do    
  let 
    corenlp_output = "Oscar/PERSON Munoz/PERSON is/O a/O CEO/O of/O United/ORGANIZATION Airlines/ORGANIZATION ./O Munoz/PERSON apologized/O to/O Dao/PERSON ./O"
    tokens   = map parseStanfordNE (C.parseNEROutputStr corenlp_output)
  eassertEqual (mergeTokens tokens) [NamedEntity "Oscar Munoz" Person, NamedEntity "United Airlines" Org, NamedEntity "Munoz" Person, NamedEntity "Dao" Person]

unitTestsAll =
  testGroup
    "Entity linking with CoreNLP named entities"
    [testNEParsing, testContextedEntityLinkingImple, testContextedEntityLinking]

main = defaultMain unitTestsAll
