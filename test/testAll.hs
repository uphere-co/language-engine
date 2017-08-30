module Main where

import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)

import qualified Test.NamedEntityTagger  as NET
import qualified Test.WikiEntityTagger   as WET
import qualified Test.CoreNLPEntity      as CNE
import qualified Test.RDFDumpETL         as RDF
import qualified Test.GraphOps           as GO

unitTests :: TestTree
unitTests =
  testGroup
    "All Unit tests"
    [ NET.allTest
    , WET.allTest
    , CNE.allTest
    --, RDF.allTest
    , GO.allTest
    ]

main = defaultMain unitTests
main1 = NET.main1
