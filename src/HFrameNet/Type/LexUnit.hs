{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type.LexUnit where

import           Control.Lens
import           Data.Text
--
import           HFrameNet.Type.Common



data Document = Document { _doc_ID :: Int
                         , _doc_name :: Text
                         , _doc_description :: Text }
              deriving (Show)

makeLenses ''Document

data Corpus = Corpus { _corpus_document :: [Document] }
            deriving (Show)

makeLenses ''Corpus                     


data Header = Header { _header_corpus :: [Corpus] }
            deriving (Show)

makeLenses ''Header



data LexUnit = LexUnit { _lexunit_header :: Header
                       , _lexunit_totalAnnotated :: Int
                       }
             deriving (Show)

makeLenses ''LexUnit
