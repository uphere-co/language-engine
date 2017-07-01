{-# LANGUAGE TemplateHaskell #-}

module VerbNet.Type where

import           Control.Lens
import           Data.Text
import           Text.Taggy.Lens

data Description = Description { _desc_primary           :: Text
                               , _desc_secondary         :: Maybe Text
                               , _desc_descriptionNumber :: Text
                               , _desc_xtag              :: Text
                               }
                 deriving Show

makeLenses ''Description


data SelRestr = SelRestr { _selrestr_value :: Text
                         , _selrestr_type :: Text }
              deriving Show

makeLenses ''SelRestr

data SelRestrs = SelRestrs { _selrestrs_elements :: [Either SelRestr SelRestrs] }
               deriving Show

makeLenses ''SelRestrs


data SynRestr = SynRestr { _synrestr_value :: Text
                         , _synrestr_type :: Text
                         }
              deriving Show

makeLenses ''SynRestr

data AndOr = And | Or
           deriving Show

data SynRestrs = SynRestrs { _synrestrs_elements :: [SynRestr]
                           , _synrestrs_logic :: Maybe AndOr
                           }
               deriving Show

makeLenses ''SynRestrs



data Syntax = NP   { _np_restrs :: Either SynRestrs SelRestrs
                   , _np_value :: Text
                   }
            | VERB
            | ADJ
            | ADV
            | PREP { _prep_value :: Maybe Text }
            | LEX  { _lex_value :: Text }
            deriving Show


data ArgType = Arg_Event
             | Arg_ThemRole
             | Arg_VerbSpecific
             | Arg_Constant
             deriving (Show)
                      

data Arg = Arg { _arg_type :: ArgType
               , _arg_value :: Text
               }
         deriving Show

makeLenses ''Arg                  

data Pred = Pred { _pred_args :: [Arg]
                 , _pred_bool :: Maybe Text
                 , _pred_value :: Text
                 }
          deriving Show

makeLenses ''Pred                   



data Frame = Frame { _frame_description :: Description
                   , _frame_examples    :: [Text]
                   , _frame_syntax      :: [Syntax]
                   , _frame_semantics   :: [Pred]
                   }
           deriving (Show)


makeLenses ''Frame                    

data Member = Member { _member_name     :: Text
                     , _member_wn       :: Text
                     , _member_grouping :: Maybe Text
                     }
            deriving Show

makeLenses ''Member                     

data ThemRole = ThemRole { _themrole_selrestrs :: SelRestrs
                         , _themrole_type :: Text
                         }
            deriving Show

makeLenses ''ThemRole                     


data VNSubclass = VNSubclass { _vnsubclass_members   :: [Member]
                             , _vnsubclass_themroles :: [ThemRole]
                             , _vnsubclass_frames    :: [Frame]
                             , _vnsubclass_subclasses :: [VNSubclass]
                             , _vnsubclass_id        :: Text
                             }
                deriving Show

makeLenses ''VNSubclass                         


data VNClass = VNClass { _vnclass_members    :: [Member]
                       , _vnclass_themroles  :: [ThemRole]
                       , _vnclass_frames     :: [Frame]
                       , _vnclass_subclasses :: [VNSubclass]
                       , _vnclass_id         :: Text
                       }
             deriving Show

makeLenses ''VNClass                        

