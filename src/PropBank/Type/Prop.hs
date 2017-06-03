{-# LANGUAGE TemplateHaskell #-}

module PropBank.Type.Prop where

import           Control.Lens
import           Data.Text                   (Text)

data IsOmit = NoOmit | Omit

data Node = Node { _node_id :: Int
                 , _node_height :: Int }
          deriving (Show,Eq,Ord)

makeLenses ''Node

data Argument = Argument { _arg_terminals :: [Node]
                         , _arg_label       :: Text
                         }
              deriving (Show,Eq,Ord)

makeLenses ''Argument
                       
data Instance = Instance { _inst_tree_id      :: Int
                         , _inst_predicate_id :: Int
                         , _inst_annotator_id :: Text
                         , _inst_lemma_type       :: Text
                         , _inst_lemma_roleset_id :: Text
                         , _inst_arguments :: [Argument]
                         }
              deriving (Show,Eq,Ord)

makeLenses ''Instance


data NomInstance = NomInstance { _nominst_tree_file    :: FilePath
                               , _nominst_tree_id      :: Int
                               , _nominst_predicate_id :: Int
                               , _nominst_base_form    :: Text
                               , _nominst_sense_number :: Int
                               , _nominst_arguments :: [Argument]
                               }
                 deriving (Show,Eq,Ord)

makeLenses ''NomInstance
