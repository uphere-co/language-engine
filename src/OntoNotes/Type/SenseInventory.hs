{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.Type.SenseInventory where

import           Control.Lens
import           Data.Text            (Text)
--
import           Lexicon.Type

data WordMeta = WordMeta { _word_meta_authors :: Text
                         , _word_meta_sample_score :: Maybe Text
                         }
                deriving Show

data SenseMeta = SenseMeta { _sense_meta_clarity :: Maybe Text }
                deriving Show
                         
makeLenses ''SenseMeta

data WN = WN { _wn_contents :: Text
             , _wn_version :: Text
             , _wn_lemma :: Maybe Text
             }
        deriving Show

makeLenses ''WN
             

data Mappings = Mappings { _mappings_gr_sense :: Maybe Text
                         , _mappings_wn       :: [WN]
                         , _mappings_omega    :: Text
                         , _mappings_pb       :: Text
                         , _mappings_vn       :: Maybe Text
                         , _mappings_fn       :: Maybe Text
                         }
              deriving Show

makeLenses ''Mappings


data Sense = Sense { _sense_commentary :: Maybe Text
                   , _sense_examples :: Text
                   , _sense_mappings :: Mappings
                   , _sense_sense_meta :: SenseMeta
                   , _sense_n     :: Text
                   , _sense_type  :: Maybe Text
                   , _sense_name  :: Text
                   , _sense_group :: Text
                   }
           deriving Show

makeLenses ''Sense

data Inventory = Inventory { _inventory_commentary :: Maybe Text
                           , _inventory_senses     :: [Sense]
                           , _inventory_lemma      :: Text
                           }
               deriving Show

makeLenses ''Inventory
