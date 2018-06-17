{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module WikiEL.Type.Wikidata where

import           Control.Lens                          (makePrisms)
import           Data.Aeson
import           Data.Hashable
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)

{-|
  Each Wikidata entity has an unique ID, e.g. Q30 for USA.
  ItemID is for representing the ID.
  PropertyID is for Wikidata properties.
  ItemRepr is for a name or an alias of Wikidata entity.
-}

data ItemID = QID { _qitemID :: Int }
            | CID { _citemID :: Int }
            deriving (Eq,Ord,Generic)

makePrisms ''ItemID

instance Hashable ItemID

instance ToJSON ItemID where
  toJSON = genericToJSON defaultOptions

instance FromJSON ItemID where
  parseJSON = genericParseJSON defaultOptions

instance Show ItemID where
  show (QID i) = "Q" ++ show i
  show (CID i) = "C" ++ show i


newtype PropertyID = PropertyID { _propID :: Int }
               deriving (Eq, Ord)
instance Show PropertyID where
  show (PropertyID uid) = "P" ++ show uid


newtype ItemRepr = ItemRepr { _repr :: Text}
                 deriving (Show, Eq, Ord)

