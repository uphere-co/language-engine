module Main where

import           NLP.Syntax.Format
--
import qualified Test.Clause       as T.C
import qualified Test.VerbProperty as T.V
--
import           Test.Tasty

main :: IO ()
main = defaultMain T.V.unitTests


