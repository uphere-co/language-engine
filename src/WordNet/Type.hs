{-# LANGUAGE TemplateHaskell #-}

module WordNet.Type where

import Control.Lens
import Data.Text (Text)

data LexItem = LexItem { _lex_lemma :: Text
                       , _lex_pos :: Text
                       , _lex_synset_cnt :: Int
                       , _lex_p_cnt :: Int
                       , _lex_ptr_symbol :: [Text]
                       , _lex_sense_cnt :: Int
                       , _lex_tagsense_cnt :: Int
                       , _lex_synset_offset :: [Int]
                       }
             deriving (Show)

makeLenses ''LexItem


