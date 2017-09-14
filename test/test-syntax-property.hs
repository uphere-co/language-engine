module Main where

--
-- import qualified Test.Clause         as T.C
import qualified Test.Trace          as T.T
import qualified Test.VerbComplement as T.V.C 
import qualified Test.VerbProperty   as T.V.P
--
import           Test.Tasty


tests :: TestTree
tests = testGroup "Syntax property unit tests" [ T.V.P.unitTests
                                               , T.V.C.unitTests
                                               ]


main :: IO ()
main = defaultMain tests


