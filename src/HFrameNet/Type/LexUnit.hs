{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type.LexUnit where

import Control.Lens

{- 
data LexUnit = LexUnit { _lexunit_header :: Header
                       , _lexunit
                       }
             deriving (Show)
-}
makeLenses ''LexUnit
