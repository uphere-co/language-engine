{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PropBank.Type.Frame where

import           Control.Lens
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           YAML.Builder

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


data VNTheta = Actor
             | Actor1
             | Actor2
             | Agent
             | Asset
             | Attribute
             | Beneficiary
             | Cause
             | CoAgent
             | CoPatient
             | CoTheme
             | Destination
             | Experiencer
             | Extent
             | Goal
             | InitialLocation
             | Instrument
             | Location
             | Material
             | Patient
             | Patient1
             | Patient2
             | Pivot
             | Predicate
             | Product
             | Proposition
             | Recipient
             | Reflexive
             | Result
             | Source
             | Stimulus
             | Theme
             | Theme1
             | Theme2
             | Time
             | Topic
             | Trajectory
             | Value
             deriving (Show,Ord,Eq)

data VNRole = VNRole { _vnrole_vncls :: Text
                     , _vntheta :: VNTheta
                     }
            deriving Show

makeLenses ''VNRole

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


instance MakeYaml Int where
  makeYaml _ x = YPrim (YInteger x)

instance MakeYaml (Int,Int) where
  makeYaml n (x,y) = YLArray Inline [ makeYaml n x, makeYaml n y ] 

instance MakeYaml Text where
  makeYaml _ txt = YPrim (YString Plain (TL.fromStrict txt))


instance MakeYaml RoleSet where
  makeYaml n s =
    YObject $
      [("id"     , makeYaml n (s^.roleset_id))]
      <> single n "name"  (s^.roleset_name)
      <> single n "source" (s^.roleset_source)
      <> single n "vncls" (s^.roleset_vncls)
      <> single n "roleset" (s^.roleset_roleset)
      <> single n "framenet" (s^.roleset_framenet)
      <> [("roles", makeYaml n (s^.roleset_roles))]

instance MakeYaml Roles where
  makeYaml n s = YIArray (map (makeYaml n) (s^.roles_role))


instance MakeYaml Role where
  makeYaml n s =
    YObject $ [("n", makeYaml n (s^.role_n))]
              <> single n "f" (s^.role_f)
              <> single n "source" (s^.role_source)
              <> single n "description" (s^.role_descr)
              <> [("vnrole", YIArray (map (makeYaml n) (s^.role_vnrole)))]

instance MakeYaml VNRole where
  makeYaml n s =
    YObject $ [ ("vncls",makeYaml n (s^.vnrole_vncls))
              , ("vntheta",makeYaml n (T.pack (show (s^.vntheta)))) 
              ]

data ProgOption = ProgOption { dir :: FilePath } deriving Show

single :: MakeYaml a => Int -> k -> Maybe a -> [(k, YamlValue)]
single n k = maybe [] (inj k . makeYaml n)

inj :: k -> a -> [(k,a)]
inj k x = [(k,x)]
