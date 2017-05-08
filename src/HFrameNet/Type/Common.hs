{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type.Common where

import           Control.Lens
import           Data.Text    (Text)

data SemType = SemType { _semtype_ID :: Int
                       , _semtype_name :: Text }
             deriving (Show)

makeLenses ''SemType

data Lexeme = Lexeme { _lexeme_name :: Text
                     , _lexeme_POS :: Text
                     , _lexeme_breakBefore :: Maybe Bool
                     , _lexeme_headword :: Maybe Bool
                     , _lexeme_order :: Maybe Int }
            deriving (Show)

makeLenses ''Lexeme

