{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.Parser.Sense where

import           Control.Lens
import           Data.Text            (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import           Data.Text.Read
import           System.FilePath


data SenseInstance = SenseInstance { _sinst_file :: Text
                                   , _sinst_sentence_id :: Int
                                   , _sinst_token_id :: Int
                                   , _sinst_sense :: Text
                                   , _sinst_sense_num :: Int
                                   }
                     deriving Show

makeLenses ''SenseInstance


parseSenseInst :: [Text] -> Either String SenseInstance
parseSenseInst ws = do
  let _sinst_file:_sinst_sentence_id':_sinst_token_id':_sinst_sense:rest = ws
  _sinst_sentence_id <- fst <$> decimal _sinst_sentence_id'
  _sinst_token_id <- fst <$> decimal _sinst_token_id'
  case rest of
    _sinst_sense_num':[]   -> fmap fst (decimal _sinst_sense_num')
                            >>= \_sinst_sense_num -> return SenseInstance {..}
    _:_sinst_sense_num':[] -> fmap fst (decimal _sinst_sense_num')
                              >>= \_sinst_sense_num -> return SenseInstance {..}
    _ -> Left "sense_num"



main :: IO ()
main = do
  let dir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj/00"
      filename = "wsj_0036.sense"

      fp = dir </> filename

  txt <- T.IO.readFile fp
  let lst = T.lines txt
      wss = map T.words lst
  mapM_ (print . parseSenseInst) wss
