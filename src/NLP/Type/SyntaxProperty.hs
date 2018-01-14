{-# LANGUAGE DeriveGeneric #-}

module NLP.Type.SyntaxProperty where

import           Data.Aeson
import           Data.Binary    (Binary)
import           Data.Hashable  (Hashable)
import           GHC.Generics   (Generic)


data Tense = Present | Past
           deriving (Show,Eq,Ord,Enum,Bounded,Generic)

instance Hashable Tense
instance ToJSON Tense
instance FromJSON Tense
instance Binary Tense

data Voice = Active | Passive
           deriving (Show,Eq,Ord,Enum,Bounded,Read,Generic)

instance Hashable Voice
instance ToJSON Voice
instance FromJSON Voice
instance Binary Voice

data Aspect = Simple | Progressive | Perfect | PerfectProgressive
           deriving (Show,Eq,Ord,Enum,Bounded,Generic)

instance Hashable Aspect
instance ToJSON Aspect
instance FromJSON Aspect
instance Binary Aspect
