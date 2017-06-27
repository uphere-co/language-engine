{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Types.Wikidata where

newtype ItemID = ItemID { _itemID :: Int }
               deriving (Eq, Ord)

instance Show ItemID where
  show (ItemID uid) = "Q" ++ show uid


newtype PropertyID = PropertyID { _propID :: Int }
               deriving (Eq, Ord)
instance Show PropertyID where
  show (PropertyID uid) = "P" ++ show uid
