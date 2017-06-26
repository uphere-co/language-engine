module NLP.Type.WordNet where

data POS = POS_N | POS_V | POS_A | POS_R deriving (Show,Eq,Ord)

data SSType = Noun | Verb | Adjective | Adverb | AdjectiveSatellite
            deriving (Show,Eq,Ord,Enum)
