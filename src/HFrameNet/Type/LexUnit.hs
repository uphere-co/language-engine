{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type.LexUnit where

import           Control.Lens
import           Data.Text
--
import           HFrameNet.Type.Common



data Document = Document { _doc_ID :: Int
                         , _doc_name :: Text
                         , _doc_description :: Text }
              deriving (Show)

makeLenses ''Document

data Corpus = Corpus { _corpus_document :: [Document] }
            deriving (Show)

makeLenses ''Corpus                     


data Header = Header { _header_corpus :: [Corpus] }
            deriving (Show)

makeLenses ''Header

data AnnoSet = AnnoSet { _anno_ID :: Int }
             deriving (Show)

makeLenses ''AnnoSet


data Governor = Governor { _gov_annoSet :: [AnnoSet]
                         , _gov_lemma :: Text
                         , _gov_type :: Text
                         }
              deriving (Show)

makeLenses ''Governor

data ValenceUnit = ValenceUnit { _vu_FE :: Text
                               , _vu_PT :: Text
                               , _vu_GF :: Text
                               }
                 deriving (Show)

makeLenses ''ValenceUnit

data Pattern = Pattern { _patt_valenceUnit :: Maybe ValenceUnit
                       , _patt_annoSet :: [AnnoSet]
                       , _patt_total :: Int
                       }
             deriving (Show)

makeLenses ''Pattern

data FEValence = FEValence { _feval_name :: Text }
               deriving (Show)

makeLenses ''FEValence                        
  
data FERealization = FERealization { _fereal_FE :: Maybe FEValence
                                   , _fereal_pattern :: [Pattern]
                                   , _fereal_total :: Int
                                   }
                   deriving (Show)

makeLenses ''FERealization 


data FEGroupRealization = FEGroupRealization { _fegroup_FE :: [FEValence]
                                             , _fegroup_pattern :: [Pattern]
                                             , _fegroup_total :: Int
                                             }
                        deriving (Show)

makeLenses ''FEGroupRealization 



data Valences = Valences { _val_governor :: [Governor]
                         , _val_FERealization :: [FERealization]
                         , _val_FEGroupRealization :: [FEGroupRealization]
                         }
              deriving (Show)

makeLenses ''Valences


data LexUnit = LexUnit { _lexunit_header :: Header
                       , _lexunit_definition :: Text
                       , _lexunit_lexeme :: [Lexeme]
                       , _lexunit_semType :: [SemType]
                       , _lexunit_valences :: Maybe Valences
                       , _lexunit_totalAnnotated :: Int
                       }
             deriving (Show)

makeLenses ''LexUnit
