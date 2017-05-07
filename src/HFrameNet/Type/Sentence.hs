{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type.Sentence where

import           Control.Lens
import           Data.Text
import           Data.Time.Clock  (UTCTime)
--
import           HFrameNet.Type.Common

-- data IType = APos | CNI | INI | DNI | INC

data Label = Label { _label_name :: Text
                   , _label_start :: Maybe Int
                   , _label_end :: Maybe Int
                   , _label_fgColor :: Maybe Text 
                   , _label_bgColor :: Maybe Text
                   {- , _label_itype -}
                   }
           deriving (Show)
                    
makeLenses ''Label

data Layer = Layer { _layer_label :: [Label]
                   , _layer_name :: Text
                   , _layer_rank :: Maybe Int
                   }
           deriving (Show)

makeLenses ''Layer                    
                 
data AnnotationSet = AnnotationSet { _annoset_layer :: [Layer]
                                   {- , _annoset_ID :: Int
                                   , _annoset_status :: Text
                                   , _annoset_frameName :: Text
                                   , _annoset_frameID :: Int
                                   , _annoset_luName :: Text
                                   , _annoset_luID :: Int
                                   , _annoset_cxnName :: Text
                                   , _annoset_cxnID :: Int
                                   , _annoset_cDate :: UTCTime -}
                                   }
                   deriving (Show)

makeLenses ''AnnotationSet


data Sentence = Sentence { _sent_text :: Text
                         , _sent_annotationSet :: [AnnotationSet]
                         {- 
                         , _sent_ID :: Int
                         , _sent_aPos :: ExtSentRef
                         , _sent_paragNo :: Int
                         , _sent_sentNo :: Int
                         , _sent_docID :: Int
                         , _sent_corpID :: Int
                         , _sent_externalID :: Text -}
                         }
              deriving (Show)

makeLenses ''Sentence
