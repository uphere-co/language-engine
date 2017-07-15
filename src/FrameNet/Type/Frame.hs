{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FrameNet.Type.Frame where

import           Control.Lens
import           Data.Text      (Text)
import           Data.Time.Clock
import           GHC.Generics
--
import           FrameNet.Type.Common


type DateTime = UTCTime

data FE = FE { _fe_ID :: Int
             , _fe_name :: Text
             , _fe_abbrev :: Text
             , _fe_cDate :: DateTime
             , _fe_coreType :: CoreType
             , _fe_fgColor :: Text
             , _fe_bgColor :: Text
             , _fe_definition :: Text
             , _fe_semType :: [SemType]
             }
          deriving (Show,Eq,Ord,Generic)

makeLenses ''FE

data MemberFE = MemberFE { _memberfe_ID :: Int
                          , _memberfe_name :: Text
                          }
              deriving (Show,Eq,Ord,Generic)

makeLenses ''MemberFE

data FEcoreSet = FEcoreSet { _fecore_memberFE :: [MemberFE]
                           }
               deriving (Show,Eq,Ord,Generic)

makeLenses ''FEcoreSet

data RelatedFrame = RelatedFrame { _relframe_ID :: Int
                                 , _relframe_content :: Text
                                 }
                  deriving (Show,Eq,Ord,Generic)

makeLenses ''RelatedFrame

data FrameRelation = FrameRelation { _frel_type :: Text
                                   , _frel_relatedFrame :: [RelatedFrame]
                                   }
                   deriving (Show,Eq,Ord,Generic)
                           
makeLenses ''FrameRelation


data SentenceCount = SentenceCount { _scount_total :: Int
                                   , _scount_annotated :: Int }
                   deriving (Show,Eq,Ord,Generic)

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
               deriving (Show,Eq,Ord,Generic)

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
           deriving (Show,Eq,Ord,Generic)

makeLenses ''Frame


