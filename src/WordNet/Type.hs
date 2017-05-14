{-# LANGUAGE TemplateHaskell #-}

module WordNet.Type where

import Control.Lens
import Data.Text (Text)

data IndexItem = IndexItem { _idx_lemma :: Text
                           , _idx_pos :: Text
                           , _idx_synset_cnt :: Int
                           , _idx_p_cnt :: Int
                           , _idx_ptr_symbol :: [Text]
                           , _idx_sense_cnt :: Int
                           , _idx_tagsense_cnt :: Int
                           , _idx_synset_offset :: [Int]
                           }
               deriving (Show)

makeLenses ''IndexItem


data SSType = Noun | Verb | Adjective | AdjectiveSatellite | Adverb
            deriving (Show,Eq,Ord)

data LexItem = LI { _lex_word :: Text, _lex_id :: Int } deriving Show

data Pointer = Pointer { _ptr_pointer_symbol :: Text
                       , _ptr_synset_offset :: Int
                       , _ptr_pos :: Text
                       , _ptr_sourcetarget :: (Text,Text) }
             deriving (Show)

makeLenses ''Pointer                      

data Frame = Frame { _frame_f_num :: Int
                   , _frame_w_num :: Int
                   } deriving Show

data DataItem = DataItem { _data_syn_offset :: Int
                         , _data_lex_filenum :: Int
                         , _data_ss_type :: SSType
                         , _data_word_lex_id :: [LexItem]
                         , _data_ptr :: [Pointer]
                         , _data_frames :: [Frame]
                         , _data_gloss :: Text }
                    deriving (Show)

makeLenses ''DataItem                             
                                           
