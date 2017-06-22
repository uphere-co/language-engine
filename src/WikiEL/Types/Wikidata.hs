{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Types.Wikidata where

newtype ItemID = ItemID {_id :: Int }
               deriving (Eq, Ord)

instance Show ItemID where
  show (ItemID uid) = "Q" ++ show uid
