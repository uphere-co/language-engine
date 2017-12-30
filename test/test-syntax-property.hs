module Main where

--
-- import qualified Test.Clause         as T.C
import qualified Test.Noun            as N
import qualified Test.Trace           as T
import qualified Test.Verb.Complement as V.C 
import qualified Test.Verb.Property   as V.P
--
import           Test.Tasty


tests :: TestTree
tests = testGroup "Syntax property unit tests" [ V.P.unitTests
                                               , V.C.unitTests
                                               -- , T.unitTests
                                               , N.unitTests
                                               ]


main :: IO ()
main = defaultMain tests


