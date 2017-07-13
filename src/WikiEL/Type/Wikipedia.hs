{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.Wikipedia where

newtype PageID = PageID {_id :: Int }
               deriving (Eq, Ord)

instance Show PageID where
  show (PageID uid) = "W" ++ show uid

newtype PageTitle = PageTitle {_title :: Int }
               deriving (Eq, Ord)

instance Show PageTitle where
  show (PageTitle title) = "enwiki/" ++ show title
