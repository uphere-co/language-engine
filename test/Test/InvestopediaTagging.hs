module Test.InvestopediaTagging where


import           Data.Maybe                            (mapMaybe)
import qualified Data.Text.IO                  as T.IO
import qualified Data.Text                     as T

import qualified WikiEL.Investopedia           as WI

import           Test.Data.Filename

main = do 
  tagger <- WI.loadInvestopediaTagger investopediaTermFile
  texts <- T.IO.readFile titleNewsSet1
  let
    lines = T.lines texts
    tagLine text = f terms
      where
        terms = tagger text
        f (Just terms) = Just (terms, text)
        f Nothing      = Nothing
  mapM_ print $ mapMaybe tagLine lines
