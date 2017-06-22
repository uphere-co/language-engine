{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Types.Wikipedia where

newtype PageID = PageID {_id :: Int }
               deriving (Eq, Ord)

instance Show PageID where
  show (PageID uid) = "W" ++ show uid
