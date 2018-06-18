{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Mapping.Addition where

import           Data.Text             (Text)
import           NLP.Type.SyntaxProperty
--
import           Lexicon.Type          (ArgPattern(..),GArg(..),GRel(..)
                                       ,POSVorN(..),SenseID)
--
import           Prelude        hiding (null)

active :: Maybe Voice
active = Just Active

npSBJ :: Maybe GRel
npSBJ = Just (GR_NP (Just GASBJ))

np1 :: Maybe GRel
np1   = Just (GR_NP (Just GA1))

np2 :: Maybe GRel
np2   = Just (GR_NP (Just GA2))

null :: Maybe GRel
null = Nothing

pp :: Text -> Maybe GRel
pp x = Just (GR_PP (Just (x,False)))

additionalMapping :: [(SenseID,ArgPattern Voice GRel)]
additionalMapping =
  --                  ONSense               Voice  arg0  arg1      arg2   arg3  arg4
  [ (("name"   ,Verb,   "1.3"), ArgPattern active npSBJ   np1 (pp "as")   null  null)
  , (("appoint",Verb,   "1.1"), ArgPattern active npSBJ   np1 (pp "as")   null  null)
  ]

