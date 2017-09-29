{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module WikiEL.Type.Wikidata where

import           Data.Aeson
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)

{-|
  Each Wikidata entity has an unique ID, e.g. Q30 for USA.
  ItemID is for representing the ID.
  PropertyID is for Wikidata properties.
  ItemRepr is for a name or an alias of Wikidata entity.
-}

newtype ItemID = ItemID { _itemID :: Int }
               deriving (Eq,Ord,Generic)

instance ToJSON ItemID where
  toJSON = genericToJSON defaultOptions

instance FromJSON ItemID where
  parseJSON = genericParseJSON defaultOptions

instance Show ItemID where
  show (ItemID uid) = "Q" ++ show uid


newtype PropertyID = PropertyID { _propID :: Int }
               deriving (Eq, Ord)
instance Show PropertyID where
  show (PropertyID uid) = "P" ++ show uid


newtype ItemRepr = ItemRepr { _repr :: Text}
                 deriving (Show, Eq, Ord)

