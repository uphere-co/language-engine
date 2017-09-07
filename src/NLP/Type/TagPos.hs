{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NLP.Type.TagPos where

import           Control.Lens
import           Control.Monad                                 (guard)
import           Data.Aeson
import           Data.Aeson.Types                              (typeMismatch)
import           Data.Maybe                                    (fromJust,mapMaybe)
import           Data.Scientific                               (floatingOrInteger)
import           Data.Text                                     (Text)
import           GHC.Generics                                  (Generic)
--


type SentIdx = Int

newtype TokIdx = TokIdx { unTokIdx :: Int } deriving (Num,Eq,Ord,Show,Generic)

newtype CharIdx = ChIdx { unChIdx :: Int } deriving (Num,Eq,Ord,Show,Generic)

type BeginEnd i = (i,i)

newtype TagPos i a = TagPos (i,i,a) deriving (Show,Generic)

instance Functor (TagPos i) where
  fmap f (TagPos (i,j,x)) = TagPos (i,j,f x)

instance FromJSON CharIdx where
  parseJSON x@(Number n) = case floatingOrInteger n :: Either Double Int of
                             Right i -> return (ChIdx i)
                             Left  _ -> typeMismatch "error in CharIdx" x
  parseJSON o            = typeMismatch "error in CharIdx" o

instance ToJSON CharIdx where
  toJSON x = toJSON (unChIdx x)

instance FromJSON TokIdx where
  parseJSON x@(Number n) = case floatingOrInteger n :: Either Double Int of
                             Right i -> return (TokIdx i)
                             Left  _ -> typeMismatch "error in TokIdx" x
  parseJSON o            = typeMismatch "error in TokIdx" o

instance ToJSON TokIdx where
  toJSON x = toJSON (unTokIdx x)

instance FromJSON (TagPos TokIdx (Maybe Text)) where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON (TagPos TokIdx (Maybe Text)) where
  toJSON = genericToJSON defaultOptions


type SentItem i = (SentIdx,BeginEnd i,Text)



