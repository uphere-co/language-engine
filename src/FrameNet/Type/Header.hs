{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FrameNet.Type.Header where

import           Control.Lens
import           Data.Text
import           GHC.Generics
--
import           FrameNet.Type.Common


data Document = Document { _doc_ID :: Int
                         , _doc_name :: Text
                         , _doc_description :: Text }
              deriving (Show,Eq,Ord,Generic)

makeLenses ''Document

data Corpus = Corpus { _corpus_document :: [Document] }
            deriving (Show,Eq,Ord,Generic)

makeLenses ''Corpus

data FE = FE { _fe_name :: Text
             -- , _fe_abbrev :: Maybe Text
             -- , _fe_type :: CoreType
             -- , _fe_bgColor :: Text
             -- , _fe_fgColor :: Text
             }
        deriving (Show,Eq,Ord,Generic)

makeLenses ''FE

{- 
data Frame = Frame { _frame_fes :: [FE] }
           deriving (Show,Eq,Ord,Generic)

makeLenses ''Frame                    
-}

data Header = Header { _header_corpus :: [Corpus]
                     , _header_fes :: [FE]
                     }
            deriving (Show,Eq,Ord,Generic)
