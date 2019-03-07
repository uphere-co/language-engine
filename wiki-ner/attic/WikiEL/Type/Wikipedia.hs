{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.Wikipedia where


{-|
  Each Wikipedia entity has their own page with a unique title.
  Internally, each title has an unique page ID, too. 
  This IDs are appeared in Wikipedia dumps.
  PageTitle is for representing Wikipedia title.
  PageID is for representing a page id of a title.
-}

newtype PageID = PageID {_id :: Int }
               deriving (Eq, Ord)

instance Show PageID where
  show (PageID uid) = "W" ++ show uid

newtype PageTitle = PageTitle {_title :: Int }
               deriving (Eq, Ord)

instance Show PageTitle where
  show (PageTitle title) = "enwiki/" ++ show title
