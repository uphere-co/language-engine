{-# LANGUAGE TemplateHaskell #-}

module PropBank.Type where

import Control.Lens
import Data.Text

data Note = Note { _note_content :: Text }
            deriving Show

makeLenses ''Note



data Person = Third | Other                  deriving (Show,Eq,Ord)

data Tense = Present | Past | Future         deriving (Show,Eq,Ord)

data Aspect = Perfect | Progressive | Both   deriving (Show,Eq,Ord)

data Voice = Active | Passive                deriving (Show,Eq,Ord)

data Form = Infinitive | Gerund | Participle | Full
          deriving (Show,Eq,Ord)

data Inflection = Inflection { _inflection_person :: Maybe Person
                             , _inflection_tense  :: Maybe Tense
                             , _inflection_aspect :: Maybe Aspect
                             , _inflection_voice  :: Maybe Voice
                             , _inflection_form   :: Maybe Form }
                deriving Show

makeLenses ''Inflection

data Arg = Arg { _arg_contents :: Text
               , _arg_n :: Text
               , _arg_h :: Maybe Text
               , _arg_f :: Maybe Text
               }
         deriving Show
                  
makeLenses ''Arg

data Rel = Rel { _rel_contents :: Text
               , _rel_n :: Maybe Text
               , _rel_h :: Maybe Text
               , _rel_f :: Maybe Text
               }
         deriving Show

makeLenses ''Rel

data Example = Example { _example_inflection :: Maybe Inflection
                       , _example_note :: [Note]
                       , _example_text :: Text
                       , _example_arg :: [Arg]
                       , _example_rel :: [Rel]
                       , _example_name :: Maybe Text
                       , _example_type :: Maybe Text
                       , _example_src :: Maybe Text
                       }
             deriving Show

makeLenses ''Example


data VNTheta = Actor1
             | Actor2
             | Agent
             | Asset
             | Attribute
             | Beneficiary
             | Cause
             | Destination
             | Experiencer
             | Extent
             | Instrument
             | Location
             | Material
             | Patient
             | Patient1
             | Patient2
             | Predicate
             | Product
             | Recipient
             | Source
             | Stimulus
             | Theme
             | Theme1
             | Theme2
             | Time
             | Topic
             deriving (Show,Ord,Eq)

data VNRole = VNRole { _vnrole_vncls :: Text
                     , _vntheta :: VNTheta
                     }
            deriving Show

makeLenses ''VNTheta                     

data Role = Role { _role_vnrole :: [VNRole]
                 , _role_n :: Text
                 , _role_f :: Maybe Text
                 , _role_source :: Maybe Text
                 , _role_descr :: Maybe Text
                 }
          deriving Show

makeLenses ''Role                   
                                   

data Roles = Roles { _roles_note :: [Note]
                   , _roles_role :: [Role] }
           deriving (Show)

makeLenses ''Roles                    
                                  
data RoleSet = RoleSet { _roleset_note :: [Note]
                       , _roleset_roles :: Roles
                       , _roleset_example :: [Example]
                       , _roleset_id :: Text
                       , _roleset_name :: Maybe Text
                       , _roleset_source :: Maybe Text
                       , _roleset_vncls :: Maybe Text
                       , _roleset_roleset :: Maybe Text
                       , _roleset_framenet :: Maybe Text
                       }
             deriving Show

makeLenses ''RoleSet
                                             


data Predicate = Pred { _predicate_note :: [Note]
                      , _predicate_roleset :: [RoleSet]
                      , _predicate_lemma :: Text
                      }
               deriving Show
                        
makeLenses ''Predicate

data FrameSet = FrameSet { _frameset_note :: [Note]
                         , _frameset_predicate :: [Predicate] }
              deriving Show

makeLenses ''FrameSet

