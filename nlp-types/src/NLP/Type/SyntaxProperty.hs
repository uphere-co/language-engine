{-# LANGUAGE DeriveGeneric #-}

module NLP.Type.SyntaxProperty where

import           Control.DeepSeq (NFData)
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
instance NFData Tense


data Voice = Active | Passive
           deriving (Show,Eq,Ord,Enum,Bounded,Read,Generic)

instance Hashable Voice
instance ToJSON Voice
instance FromJSON Voice
instance Binary Voice
instance NFData Voice


data Aspect = Simple | Progressive | Perfect | PerfectProgressive
           deriving (Show,Eq,Ord,Enum,Bounded,Generic)

instance Hashable Aspect
instance ToJSON Aspect
instance FromJSON Aspect
instance Binary Aspect
instance NFData Aspect
