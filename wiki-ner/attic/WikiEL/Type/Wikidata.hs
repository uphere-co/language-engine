{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module WikiEL.Type.Wikidata where

import           Control.Lens                          (makePrisms)
import           Data.Aeson
import           Data.Hashable
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)



newtype PropertyID = PropertyID { _propID :: Int }
               deriving (Eq, Ord)
instance Show PropertyID where
  show (PropertyID uid) = "P" ++ show uid


