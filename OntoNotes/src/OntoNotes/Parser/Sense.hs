{-# LANGUAGE RecordWildCards #-}

module OntoNotes.Parser.Sense where

import           Data.Text            (Text)
import           Data.Text.Read
--
import           OntoNotes.Type.Sense


parseSenseInst :: [Text] -> Either String SenseInstance
parseSenseInst ws = do
  let _sinst_file:_sinst_sentence_id':_sinst_token_id':_sinst_sense:rest = ws
  _sinst_sentence_id <- fst <$> decimal _sinst_sentence_id'
  _sinst_token_id <- fst <$> decimal _sinst_token_id'
  case rest of
    _sinst_sense_num:[]   -> return SenseInstance {..}
    _:_sinst_sense_num:[] -> return SenseInstance {..}
    _ -> Left "sense_num"



