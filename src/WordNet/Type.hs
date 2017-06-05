{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Type where
{- ( module NLP.Type.WordNet
, IndexItem
, LexItem
, Pointer
, Frame
, DataItem
, formatLI
, idx_lemma
, idx_pos
, idx_ptr_symbol
, idx_tagsense_cnt
, idx_synset_offset
, lex_word
, lex_id
, ptr_pointer_symbol
, ptr_synset_offset
, ptr_pos
, ptr_sourcetarget
, frame_f_num
, frame_w_num
, data_syn_offset
, data_lex_filenum
, data_ss_type
, data_word_lex_id
, data_ptr
, data_frames
, data_gloss
) where -}

import           Control.Lens
import           Data.Text             (Text)
import           Data.Text.Format 
import qualified Data.Text.Lazy  as TL
--
import           NLP.Type.WordNet

data IndexItem = IndexItem { _idx_lemma :: Text
                           , _idx_pos :: POS
                           -- , _idx_synset_cnt :: Int
                           , _idx_ptr_symbol :: [Text]
                           -- , _idx_sense_cnt :: Int   --  sense_cnt = synset_cnt
                           , _idx_tagsense_cnt :: Int
                           , _idx_synset_offset :: [Int] -- length = synset_cnt
                           }
               deriving (Show)

makeLenses ''IndexItem

data LexItem = LI { _lex_word :: Text, _lex_id :: Int } deriving Show

formatLI :: LexItem -> Text
formatLI (LI w i) = TL.toStrict $ format "{}.{}" (w,i)


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
                                           
data SenseItem = SenseItem { _sense_lemma :: Text
                           , _sense_ss :: Int
                           , _sense_lexfilenum :: Int
                           , _sense_lexid :: Int
                           , _sense_headword :: Text
                           , _sense_headid :: Int
                           , _sense_soffset :: Int
                           , _sense_snumber :: Int
                           , _sense_cnt :: Int
                           } deriving (Show)

makeLenses ''SenseItem

  
