{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type.Common where

import           Control.Lens
import           Data.Text    (Text)


data Lexeme = Lexeme { _lexeme_name :: Text
                     , _lexeme_POS :: Text
                     , _lexeme_breakBefore :: Bool
                     , _lexeme_headword :: Bool
                     , _lexeme_order :: Int }
            deriving (Show)

makeLenses ''Lexeme

