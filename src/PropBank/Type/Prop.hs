{-# LANGUAGE TemplateHaskell #-}

module PropBank.Type.Prop where

import           Control.Lens
import           Data.Text                   (Text)

data IsOmit = NoOmit | Omit

{- 
data NumberedArguments = ARG0
                       | ARG1
                       | ARG2
                       | ARG3
                       | ARG4
-}

data ModifierType = ADJ  -- ^ Adjectivals (modifies nouns)
                  | ADV  -- ^ Adverbials (modifies verbs)
                  | CAU  -- ^ Causatives
                  | COM  -- ^ Comitatives
                  | DIR  -- ^ Directionals
                  | DIS  -- ^ Discourse markers
                  | DSP  -- ^ Direct speech
                  | EXT  -- ^ Extents
                  | GOL  -- ^ Goals
                  | LOC  -- ^ Locatives
                  | MNR  -- ^ Manners
                  | MOD  -- ^ Modals
                  | NEG  -- ^ Negations
                  | PRD  -- ^ Secondary predications
                  | PRP  -- ^ Purpose
                  | PRR  -- ^ Nominal predicates in light verb constructions
                  | REC  -- ^ Reciprocals
                  | TMP  -- ^ Temporals
                  deriving (Show,Enum,Eq,Ord)

data LinkType     = PRO  -- ^ semantic link of the *PRO* argument if semantically recoverable in the sentence
                  | PSV  -- ^ semantic link of the passive trace to the SBJ constituent
                  | SLC
                  deriving (Show,Enum,Eq,Ord)
                    
data PropBankLabel = Relation
                   | NumberedArgument Int          -- ^ Argument roles that are semantically
                                                   --   licensed by the predicate
                   | Modifier         ModifierType -- ^ Predicate or phrasal modifiers
                   | LinkArgument     LinkType     -- ^ Labels that link two constituents together
                   deriving (Show,Eq,Ord)

data Node = Node { _node_id :: Int
                 , _node_height :: Int }
          deriving (Show,Eq,Ord)

makeLenses ''Node

data Argument = Argument { _arg_terminals :: [Node]
                         , _arg_label     :: PropBankLabel
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
