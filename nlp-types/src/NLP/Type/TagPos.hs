{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module NLP.Type.TagPos where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types                              (typeMismatch)
import           Data.Function                                 (on)
import           Data.List                                     (sortBy)
import           Data.Maybe                                    (mapMaybe)
import           Data.Scientific                               (floatingOrInteger)
import           Data.Text                                     (Text)
import           GHC.Generics                                  (Generic)
--


type SentIdx = Int

newtype TokIdx = TokIdx { unTokIdx :: Int } deriving (Num,Eq,Ord,Show,Generic)

newtype CharIdx = ChIdx { unChIdx :: Int } deriving (Num,Eq,Ord,Show,Generic)

type BeginEnd i = (i,i)

newtype TagPos i a = TagPos (i,i,a) deriving (Eq,Show,Generic)

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

instance (FromJSON a) => FromJSON (TagPos TokIdx a) where
  parseJSON = genericParseJSON defaultOptions

instance (ToJSON a) => ToJSON (TagPos TokIdx a) where
  toJSON = genericToJSON defaultOptions


type SentItem i = (SentIdx,BeginEnd i,Text)


mergeTagPos :: (Ord i) => [TagPos i a] -> [TagPos i b] -> [TagPos i (Either a b)]
mergeTagPos xs ys =
  let zs = map (fmap Left) xs ++ map (fmap Right) ys
      idx (TagPos (i,_,_)) = i
  in sortBy (compare `on` idx) zs


leftTagPos :: [TagPos i (Either a b)] -> [TagPos i a]
leftTagPos xs = mapMaybe (\(TagPos (b,e,x)) -> TagPos . (b,e,) <$> (x^?_Left)) xs


rightTagPos :: [TagPos i (Either a b)] -> [TagPos i b]
rightTagPos xs = mapMaybe (\(TagPos (b,e,x)) -> TagPos . (b,e,) <$> (x^?_Right)) xs



