{-# LANGUAGE OverloadedStrings  #-}

module SRL.Statistics where

import Control.Lens     ((^.),_1,_2,_3,_4,_5)
import Data.Maybe       (fromMaybe,mapMaybe)
import           Data.Monoid                             ((<>))

--
import NLP.Syntax.Format (formatAspect,formatTense)
import SRL.Analyze.Type (MGVertex(..),SentStructure(..),mg_vertices)


-- DocStructure mtokenss sentitems mergedtags sstrs

numberOfPredicate (SentStructure i ptr vps clausetr mcpstr vstrs) = length vstrs

numberOfMGPredicate mg = length $ mapMaybe fmtVerb (mg ^. mg_vertices)
  where
    fmtVerb (MGEntity    _ _ _  ) = Nothing
    fmtVerb (MGPredicate i _ f v) = Just i
