{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Type where

import           Control.Lens
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text        as T
import           Data.Text.Format 
import qualified Data.Text.Lazy   as TL
--
import           WordNet.Type.Lexicographer
import           WordNet.Type.POS




data IndexItem
  = IndexItem { _idx_lemma :: Text
              , _idx_pos :: POS
              , _idx_ptr_symbol :: [Text]
              , _idx_tagsense_cnt :: Int
              , _idx_synset_offset :: [Int] -- ^ length = sense_cnt = synset_cnt
              }
  deriving (Show)

makeLenses ''IndexItem

data LexItem = LI { _lex_word :: Text, _lex_id :: LexicographerID } deriving Show

formatLI :: LexItem -> Text
formatLI (LI w i) = TL.toStrict $ format "{}.{}" (w,unLex i)


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


                           
data LexSense = LexSense { _lexsens_sstype      :: SSType
                         , _lexsens_lex_filenum :: LexicographerFile
                         , _lexsens_lex_id      :: Int
                         , _lexsens_head_word   :: Text
                         , _lexsens_head_id     :: Int }
              deriving Show

makeLenses ''LexSense                       


data SenseKey = SenseKey { _skey_lemma :: Text
                         , _skey_lex_sense :: LexSense }
              deriving Show

makeLenses ''SenseKey


headWord :: LexSense -> Text
headWord l | T.null (l^.lexsens_head_word) = ""
           | otherwise = l^.lexsens_head_word <> "." <> T.pack (show (l^.lexsens_head_id))


data SenseItem = SenseItem { _sense_sense_key :: SenseKey
                           , _sense_soffset :: Int
                           , _sense_snumber :: Int
                           , _sense_cnt :: Int
                           } deriving (Show)

makeLenses ''SenseItem

  
