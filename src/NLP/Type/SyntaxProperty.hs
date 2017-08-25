{-# LANGUAGE DeriveGeneric #-}

module NLP.Type.SyntaxProperty where

import           Data.Hashable  (Hashable)
import           GHC.Generics   (Generic)


data Tense = Present | Past
           deriving (Show,Eq,Ord,Enum,Bounded)


data Voice = Active | Passive
           deriving (Show,Eq,Ord,Enum,Bounded,Read,Generic)

instance Hashable Voice


data Aspect = Simple | Progressive | Perfect | PerfectProgressive
           deriving (Show,Eq,Ord,Enum,Bounded)
