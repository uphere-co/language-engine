{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module WordNet.Type where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text        as T
import           GHC.Generics
--
import           WordNet.Type.Lexicographer
import           WordNet.Type.POS


newtype SynsetOffset = SynsetOffset { unSynsetOffset :: Int }
                     deriving (Show,Eq,Ord,Enum,Num,Real,Integral)

newtype SenseNumber = SenseNumber { unSenseNumber :: Int }
                    deriving (Show,Eq,Ord,Enum,Num,Real,Integral)


data IndexItem
  = IndexItem { _idx_lemma :: Text
              , _idx_pos :: POS
              , _idx_ptr_symbol :: [Text]
              , _idx_tagsense_cnt :: Int
              , _idx_synset_offset :: [(SenseNumber,SynsetOffset)] -- ^ length = sense_cnt = synset_cnt
              }
  deriving (Show)

makeLenses ''IndexItem

data LexItem = LI { _lex_word :: Text, _lex_id :: LexID } deriving Show



data Pointer = Pointer { _ptr_pointer_symbol :: Text
                       , _ptr_synset_offset :: SynsetOffset
                       , _ptr_pos :: POS
                       , _ptr_sourcetarget :: (Int,Int) }
             deriving (Show)

makeLenses ''Pointer                      

data Frame = Frame { _frame_f_num :: Int
                   , _frame_w_num :: Int
                   } deriving Show

makeLenses ''Frame


data DataItem = DataItem { _data_syn_offset :: SynsetOffset
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
                         , _lexsens_lex_id      :: LexID
                         , _lexsens_head_word   :: Maybe Text
                         , _lexsens_head_id     :: Maybe LexID }
              deriving Show

makeLenses ''LexSense                       


data SenseKey = SenseKey { _skey_lemma :: Text
                         , _skey_lex_sense :: LexSense }
              deriving Show

makeLenses ''SenseKey


headWord :: LexSense -> Text
headWord l = fromMaybe "" (l^.lexsens_head_word) <> "." <>
             maybe "" (T.singleton . unLexID) (l^.lexsens_head_id)



data SenseItem = SenseItem { _sense_sense_key :: SenseKey
                           , _sense_soffset :: SynsetOffset
                           , _sense_snumber :: SenseNumber
                           , _sense_cnt :: Int
                           } deriving (Show)

makeLenses ''SenseItem

  
