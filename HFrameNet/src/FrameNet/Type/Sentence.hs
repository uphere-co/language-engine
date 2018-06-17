{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------
--
-- This module follows the definition of (framenet)/schema/sentence.xsd
--
------------------------------------------------------------------------

module FrameNet.Type.Sentence where

import           Control.Lens
import           Data.Binary
import           Data.Binary.Orphans  -- for UTCTime
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.Clock
import           GHC.Generics
--

{- 
-- orphan instance
deriving instance Generic Day
deriving instance Binary Day
deriving instance Generic DiffTime
deriving instance Binary DiffTime
deriving instance Generic UTCTime
deriving instance Binary UTCTime
-}

data IType = APos | CNI | INI | DNI | INC
           deriving (Show,Eq,Ord,Enum,Bounded,Generic,Binary)


data Label = Label { _label_name :: Text
                   , _label_start :: Maybe Int
                   , _label_end :: Maybe Int
                   , _label_fgColor :: Maybe Text 
                   , _label_bgColor :: Maybe Text
                   , _label_itype :: Maybe IType
                   , _label_feID :: Maybe Int
                   , _lbael_cBy  :: Maybe Text
                   }
           deriving (Show,Eq,Ord,Generic,Binary)
                    
makeLenses ''Label

data Layer = Layer { _layer_label :: [Label]
                   , _layer_name :: Text
                   , _layer_rank :: Maybe Int
                   }
           deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''Layer                    
                 
data AnnotationSet = AnnotationSet { _annoset_layer :: [Layer]
                                   , _annoset_ID :: Maybe Int
                                   , _annoset_status :: Maybe Text
                                   , _annoset_frameName :: Maybe Text
                                   , _annoset_frameID :: Maybe Int
                                   , _annoset_luName :: Maybe Text
                                   , _annoset_luID :: Maybe Int
                                   , _annoset_cxnName :: Maybe Text
                                   , _annoset_cxnID :: Maybe Int
                                   , _annoset_cDate :: Maybe UTCTime
                                   }
                   deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''AnnotationSet


data Sentence = Sentence { _sent_text :: Text
                         , _sent_annotationSet :: [AnnotationSet]
                         , _sent_ID :: Maybe Int
                         , _sent_aPos :: Maybe Int
                         , _sent_paragNo :: Maybe Int
                         , _sent_sentNo :: Maybe Int
                         , _sent_docID :: Maybe Int
                         , _sent_corpID :: Maybe Int
                         , _sent_externalID :: Maybe Text 
                         }
              deriving (Show,Eq,Ord,Generic,Binary)

makeLenses ''Sentence
