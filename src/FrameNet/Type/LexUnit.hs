{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FrameNet.Type.LexUnit where

import           Control.Lens
import           Data.Binary
import           Data.Text
import           GHC.Generics
--
import           FrameNet.Type.Common
import           FrameNet.Type.Header
import           FrameNet.Type.Sentence


data AnnoSet = AnnoSet { _anno_ID :: Int }
             deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''AnnoSet


data Governor = Governor { _gov_annoSet :: [AnnoSet]
                         , _gov_lemma :: Text
                         , _gov_type :: Text
                         }
              deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''Governor

data ValenceUnit = ValenceUnit { _vu_FE :: Text
                               , _vu_PT :: Text
                               , _vu_GF :: Text
                               }
                 deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''ValenceUnit

data Pattern = Pattern { _patt_valenceUnit :: Maybe ValenceUnit
                       , _patt_annoSet :: [AnnoSet]
                       , _patt_total :: Int
                       }
             deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''Pattern

data FEValence = FEValence { _feval_name :: Text }
               deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''FEValence                        
  
data FERealization = FERealization { _fereal_FE :: Maybe FEValence
                                   , _fereal_pattern :: [Pattern]
                                   , _fereal_total :: Int
                                   }
                   deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''FERealization 


data FEGroupRealization = FEGroupRealization { _fegroup_FE :: [FEValence]
                                             , _fegroup_pattern :: [Pattern]
                                             , _fegroup_total :: Int
                                             }
                        deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''FEGroupRealization 



data Valences = Valences { _val_governor :: [Governor]
                         , _val_FERealization :: [FERealization]
                         , _val_FEGroupRealization :: [FEGroupRealization]
                         }
              deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''Valences


data SubCorpus = SubCorpus { _subcorp_sentence :: [Sentence]
                           , _subcorp_name :: Text
                           }
               deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''SubCorpus

data LexUnit = LexUnit { _lexunit_header :: Header
                       , _lexunit_definition :: Text
                       , _lexunit_lexeme :: [Lexeme]
                       , _lexunit_semType :: [SemType]
                       , _lexunit_valences :: Maybe Valences
                       , _lexunit_subCorpus :: [SubCorpus]
                       , _lexunit_basicLUAttributes :: BasicLUAttributes
                       , _lexunit_frameReference :: FrameReference
                       , _lexunit_totalAnnotated :: Int
                       }
             deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''LexUnit
