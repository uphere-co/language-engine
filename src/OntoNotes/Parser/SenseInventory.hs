{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.Parser.SenseInventory where

import           Control.Applicative
import           Control.Lens       hiding (element,elements)
import           Data.Text                 (Text)
import qualified Data.Text         as T
import           Data.Traversable          (traverse)
import           Text.Taggy.Lens
--
import           Text.Taggy.Lens.Util

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


data Inventory = Inventory { _inventory_commentary :: Maybe Text
                           , _inventory_senses     :: [Sense]
                           , _inventory_lemma      :: Text
                           }
               deriving Show

makeLenses ''Inventory

p_list p each group x = getOnly1 x group >>= \y -> traverse p (getOnly y each) 


p_word_meta :: Element -> Parser WordMeta
p_word_meta x = WordMeta <$> x .: "authors"
                         <*> optional (x .: "sample_score")


p_sense_meta :: Element -> Parser SenseMeta
p_sense_meta x = SenseMeta <$> optional (x .: "clarity")


p_wn :: Element -> Parser WN
p_wn x = WN <$> pure (x^.contents)
            <*> x .: "version"
            <*> optional (x .: "lemma")


p_mappings :: Element -> Parser Mappings
p_mappings x = Mappings <$> optional ((^.contents) <$> getOnly1 x "gr_sense")
                        <*> traverse p_wn (getOnly x "wn")
                        <*> ((^.contents) <$> getOnly1 x "omega")
                        <*> ((^.contents) <$> getOnly1 x "pb")
                        <*> optional ((^.contents) <$> getOnly1 x "vn")
                        <*> optional ((^.contents) <$> getOnly1 x "fn")
  

p_sense :: Element -> Parser Sense
p_sense x = Sense <$> optional ((^.contents) <$> getOnly1 x "commentary")
                  <*> ((^.contents) <$> getOnly1 x "examples")
                  <*> (p_mappings =<< getOnly1 x "mappings")
                  <*> (p_sense_meta =<< getOnly1 x "SENSE_META")
                  <*> x .: "n"
                  <*> optional (x .: "type")
                  <*> x .: "name"
                  <*> x .: "group"


p_inventory :: Element -> Parser Inventory
p_inventory x = Inventory <$> optional ((^.contents) <$> getOnly1 x "commentary")
                          <*> traverse p_sense (getOnly x "sense")
                          <*> x .: "lemma"



