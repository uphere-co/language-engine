{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module WikiEL.Type.Wikidata where

import           Data.Aeson
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)

newtype ItemID = ItemID { _itemID :: Int }
               deriving (Eq,Ord,Generic)

instance ToJSON ItemID where
  toJSON = genericToJSON defaultOptions

instance Show ItemID where
  show (ItemID uid) = "Q" ++ show uid


newtype PropertyID = PropertyID { _propID :: Int }
               deriving (Eq, Ord)
instance Show PropertyID where
  show (PropertyID uid) = "P" ++ show uid


newtype ItemRepr = ItemRepr { _repr :: Text}
                 deriving (Show, Eq, Ord)

