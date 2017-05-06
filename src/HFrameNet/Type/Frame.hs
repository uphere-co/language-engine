{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type.Frame where

import           Control.Lens
import           Data.Text      (Text)
import qualified Data.Text as T
import           Data.Time.Clock
--
import           HFrameNet.Type.Common


type DateTime = UTCTime

data SemType = SemType { _semtype_ID :: Int
                       , _semtype_name :: Text }
             deriving (Show)

makeLenses ''SemType

data FE = FE { _fe_ID :: Int
             , _fe_name :: Text
             , _fe_abbrev :: Text
             , _fe_cDate :: DateTime
             , _fe_coreType :: Text
             , _fe_fgColor :: Text
             , _fe_bgColor :: Text
             , _fe_definition :: Text
             , _fe_semType :: [SemType]
             }
          deriving (Show)

makeLenses ''FE

data MemberFE = MemberFE { _memberfe_ID :: Int
                          , _memberfe_name :: Text
                          }
              deriving (Show)

makeLenses ''MemberFE

data FEcoreSet = FEcoreSet { _fecore_memberFE :: [MemberFE]
                           }
               deriving (Show)

makeLenses ''FEcoreSet

data RelatedFrame = RelatedFrame { _relframe_ID :: Int
                                 , _relframe_content :: Text
                                 }
                  deriving (Show)

makeLenses ''RelatedFrame

data FrameRelation = FrameRelation { _frel_type :: Text
                                   , _frel_relatedFrame :: [RelatedFrame]
                                   }
                   deriving (Show)
                           
makeLenses ''FrameRelation


data SentenceCount = SentenceCount { _scount_total :: Int
                                   , _scount_annotated :: Int }
                   deriving (Show)

makeLenses ''SentenceCount

data LexUnit = LexUnit { _lexunit_ID :: Int
                       , _lexunit_name :: Text
                       , _lexunit_POS :: Text
                       , _lexunit_status :: Text
                       , _lexunit_cDate :: DateTime
                       , _lexunit_cBy :: Text
                       , _lexunit_lemmaID :: Int
                       , _lexunit_definition :: Text
                       , _lexunit_sentenceCount :: SentenceCount 
                       , _lexunit_lexeme :: [Lexeme]
                       , _lexunit_semType :: [SemType]
                                            
                       }
               deriving (Show)

makeLenses ''LexUnit


data Frame = Frame { _frame_ID :: Int
                   , _frame_name :: Text
                   , _frame_cDate :: DateTime
                   , _frame_definition :: Text
                   , _frame_FE :: [FE]
                   , _frame_FEcoreSet :: [FEcoreSet]
                   , _frame_frameRelation :: [FrameRelation]
                   , _frame_lexUnit :: [LexUnit]
                   }
           deriving (Show)

makeLenses ''Frame


