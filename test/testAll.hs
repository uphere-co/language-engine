import           Test.Tasty.HUnit                      (testCase,testCaseSteps)
import           Test.Tasty                            (defaultMain, testGroup,TestTree)

import qualified Test.NamedEntityTagger  as NET
import qualified Test.WikiEntityTagger   as WET
import qualified Test.CoreNLPEntity      as CNE
import qualified Test.RDFDumpETL         as RDF


unitTests :: TestTree
unitTests =
  testGroup
    "All Unit tests"
    [ NET.allTest
    , WET.allTest
    , CNE.allTest
    , RDF.allTest
    ]

main = defaultMain unitTests
--main = NET.main1
