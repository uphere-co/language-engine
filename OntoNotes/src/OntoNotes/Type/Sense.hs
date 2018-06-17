{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.Type.Sense where

import           Control.Lens
import           Data.Text            (Text)

data SenseInstance = SenseInstance { _sinst_file :: Text
                                   , _sinst_sentence_id :: Int
                                   , _sinst_token_id :: Int
                                   , _sinst_sense :: Text
                                   , _sinst_sense_num :: Text
                                   }
                     deriving Show

makeLenses ''SenseInstance


getSenseID :: SenseInstance -> (Text,Text)
getSenseID s = (s^.sinst_sense,s^.sinst_sense_num)
