{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module WordNet.Type.POS where

import Data.Hashable
import GHC.Generics


data POS = POS_N | POS_V | POS_A | POS_R deriving (Show,Eq,Ord,Generic)

instance Hashable POS


data SSType = Noun | Verb | Adjective | Adverb | AdjectiveSatellite
            deriving (Show,Eq,Ord,Generic)


posToSSType :: POS -> SSType
posToSSType POS_N = Noun
posToSSType POS_V = Verb
posToSSType POS_A = Adjective   -- this may not be safe.
posToSSType POS_R = Adverb


instance Enum SSType where
  toEnum 1 = Noun
  toEnum 2 = Verb
  toEnum 3 = Adjective
  toEnum 4 = Adverb
  toEnum 5 = AdjectiveSatellite
  toEnum n = error ("toEnum: got " ++ show n)

  fromEnum Noun               = 1
  fromEnum Verb               = 2
  fromEnum Adjective          = 3
  fromEnum Adverb             = 4
  fromEnum AdjectiveSatellite = 5

instance Bounded SSType where
  minBound = Noun
  maxBound = AdjectiveSatellite
