{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Type.WordNet where

import          Data.Text          (Text)

data Synset = Synset { _repr :: Text
                     , _pos  :: Text
                     , _idx  :: Int
                     }
            deriving (Eq,Show)

data SynsetY = SynsetY { _name :: Text
                       , _id  :: Int
                       }
            deriving (Eq,Show)

