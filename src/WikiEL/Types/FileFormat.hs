{-# LANGUAGE OverloadedStrings #-}

module WikiEL.Types.FileFormat where

import           Data.Text                             (Text)

import           WikiEL.Types.Wikidata


newtype PropertyNameFile = PropertyNameFile { _path :: FilePath }
                         deriving (Show)

data PropertyNameRow = PropertyNameRow { _prop     :: PropertyID                       
                                       , _propName :: Text
                                       }
                     deriving (Show)

