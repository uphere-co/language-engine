{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Wikipedia.Types where

newtype PageID = PageID {_id :: Int }
               deriving (Eq, Ord)

instance Show PageID where
  show (PageID uid) = "W" ++ show uid
