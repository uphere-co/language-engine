{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.Parser.Sense where

import           Control.Lens
import           Data.Text            (Text)
import qualified Data.Text    as T
import           Data.Text.Read


data SenseInstance = SenseInstance { _sinst_file :: Text
                                   , _sinst_sentence_id :: Int
                                   , _sinst_token_id :: Int
                                   , _sinst_sense :: Text
                                   , _sinst_sense_num :: Text
                                   }
                     deriving Show

makeLenses ''SenseInstance


parseSenseInst :: [Text] -> Either String SenseInstance
parseSenseInst ws = do
  let _sinst_file:_sinst_sentence_id':_sinst_token_id':_sinst_sense:rest = ws
  _sinst_sentence_id <- fst <$> decimal _sinst_sentence_id'
  _sinst_token_id <- fst <$> decimal _sinst_token_id'
  case rest of
    _sinst_sense_num:[]   -> return SenseInstance {..}
      --  fmap fst (decimal _sinst_sense_num')
                              -- >>= \_sinst_sense_num -> return SenseInstance {..}
    _:_sinst_sense_num:[] -> return SenseInstance {..}
                              -- fmap fst (decimal _sinst_sense_num')
                              -- >>= \_sinst_sense_num -> return SenseInstance {..}
    _ -> Left "sense_num"



