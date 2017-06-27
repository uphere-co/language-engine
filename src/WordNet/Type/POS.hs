{-# LANGUAGE DataKinds #-}

module WordNet.Type.POS where

data POS = POS_N | POS_V | POS_A | POS_R deriving (Show,Eq,Ord)

data SSType = Noun | Verb | Adjective | Adverb | AdjectiveSatellite
            deriving (Show,Eq,Ord)

instance Enum SSType where
  toEnum 1 = Noun
  toEnum 2 = Verb
  toEnum 3 = Adjective
  toEnum 4 = Adverb
  toEnum 5 = AdjectiveSatellite

  fromEnum Noun               = 1
  fromEnum Verb               = 2
  fromEnum Adjective          = 3
  fromEnum Adverb             = 4
  fromEnum AdjectiveSatellite = 5

instance Bounded SSType where
  minBound = Noun
  maxBound = AdjectiveSatellite
