{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Merge where

import           Control.Lens             ((^.),(%~),_1,_2)
import           Data.Function            (on)
import           Data.List                (groupBy,sortBy)
import           Data.Text                (Text)
--
import           NLP.Type.SyntaxProperty  (Voice(..))
--
import           Lexicon.Type             (ArgPattern(..)
                                          ,patt_property
                                          ,patt_arg0
                                          ,patt_arg1
                                          ,patt_arg2
                                          ,patt_arg3
                                          ,patt_arg4
                                          )



convertPassive :: ArgPattern Voice Text -> ArgPattern () Text
convertPassive patt =
    case patt^.patt_property of
      Just Passive -> ArgPattern
                        (Just ())
                        (conv (patt^.patt_arg0))
                        (conv (patt^.patt_arg1))
                        (conv (patt^.patt_arg2))
                        (conv (patt^.patt_arg3))
                        (conv (patt^.patt_arg4))
      _            -> ArgPattern
                        (Just ())
                        (patt^.patt_arg0)
                        (patt^.patt_arg1)
                        (patt^.patt_arg2)
                        (patt^.patt_arg3)
                        (patt^.patt_arg4)

  where
    conv (Just "PP-by")  = Just "NP-SBJ"
    conv (Just "NP-SBJ") = Just "NP-1"
    conv x               = x

mergePatterns :: [(ArgPattern Voice Text,Int)] -> [(ArgPattern () Text,Int)]
mergePatterns pattstats0 =
  let pgrps = groupBy ((==) `on` (^._1))
            . sortBy (compare `on` (^._1))
            . map (_1 %~ convertPassive)
            $ pattstats0
  in sortBy (flip compare `on` (^._2)) . map (\xs -> (fst (head xs), sum (map snd xs))) $ pgrps
