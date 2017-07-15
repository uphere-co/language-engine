{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FrameNet.Type.Sentence where

import           Control.Lens
import           Data.Text
import           Data.Time.Clock  (UTCTime)
import           GHC.Generics
--


data IType = APos | CNI | INI | DNI | INC
           deriving (Show,Eq,Ord,Enum,Bounded,Generic)


data Label = Label { _label_name :: Text
                   , _label_start :: Maybe Int
                   , _label_end :: Maybe Int
                   , _label_fgColor :: Maybe Text 
                   , _label_bgColor :: Maybe Text
                   , _label_itype :: Maybe IType
                   , _label_feID :: Maybe Int
                   , _lbael_cBy  :: Maybe Text
                   }
           deriving (Show,Eq,Ord,Generic)
                    
makeLenses ''Label

data Layer = Layer { _layer_label :: [Label]
                   , _layer_name :: Text
                   , _layer_rank :: Maybe Int
                   }
           deriving (Show,Eq,Ord,Generic)

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
                   deriving (Show,Eq,Ord,Generic)

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
              deriving (Show,Eq,Ord,Generic)

makeLenses ''Sentence
