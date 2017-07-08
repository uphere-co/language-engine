{-# LANGUAGE TemplateHaskell #-}

module SRL.Type.Verb where

import           Control.Lens
import           Data.Text                   (Text)
--
import           NLP.Type.PennTreebankII

data Tense = Present | Past
           deriving (Show,Eq,Ord,Enum,Bounded)

                    
data Voice = Active | Passive
           deriving (Show,Eq,Ord,Enum,Bounded)


data Aspect = Simple | Progressive | Perfect | PerfectProgressive
           deriving (Show,Eq,Ord,Enum,Bounded)

data VerbProperty = VerbProperty { _vp_index  :: Int
                                 , _vp_lemma  :: Lemma
                                 , _vp_tense  :: Tense
                                 , _vp_aspect :: Aspect
                                 , _vp_voice  :: Voice
                                 , _vp_auxiliary :: Maybe (Int,Lemma)
                                 , _vp_words  :: [Int]
                                 }
                  deriving (Show)

makeLenses ''VerbProperty                           

data VerbArgs a = VerbArgs { _va_string :: [(POSTag,Text)]
                           , _va_arg0 :: Maybe a
                           }
              deriving Show

makeLenses ''VerbArgs                       

