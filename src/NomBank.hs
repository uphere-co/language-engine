{-# LANGUAGE TemplateHaskell #-}

module NomBank where

import Control.Lens
import Data.Text

data Note = Note { _note_content :: Text }
            deriving Show

makeLenses ''Note

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
                       -- , _roleset_example :: [Example]
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






{- 

data Example

data Inflection

data Arg

data Rel
-}
