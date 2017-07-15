{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FrameNet.Type.Common where

import           Control.Lens
import           Data.Text    (Text)
import           GHC.Generics

data SemType = SemType { _semtype_ID :: Int
                       , _semtype_name :: Text }
             deriving (Show,Eq,Ord,Generic)

makeLenses ''SemType

data Lexeme = Lexeme { _lexeme_name :: Text
                     , _lexeme_POS :: Text
                     , _lexeme_breakBefore :: Maybe Bool
                     , _lexeme_headword :: Maybe Bool
                     , _lexeme_order :: Maybe Int }
            deriving (Show,Eq,Ord,Generic)

makeLenses ''Lexeme

data POS = A
         | ADV
         | ART
         | AVP
         | C
         | CCON
         | IDIO
         | INTJ
         | N
         | NUM
         | PREP
         | PRON
         | SCON
         | V
         deriving (Show,Eq,Ord,Enum,Bounded,Generic)

data BasicLUAttributes = BasicLUAttributes { _bluattr_ID :: Int
                                           , _bluattr_name :: Text
                                           , _bluattr_POS :: POS
                                           , _bluattr_incorporatedFE :: Maybe Text
                                           , _bluattr_status :: Maybe Text
                                           }
                       deriving (Show,Eq,Ord,Generic)

makeLenses ''BasicLUAttributes

data FrameReference = FrameReference { _fr_frameID :: Maybe Int
                                     , _fr_frame :: Maybe Text
                                     }
                    deriving (Show,Eq,Ord,Generic)

makeLenses ''FrameReference                             

data CoreType = Core | Peripheral | ExtraThematic | CoreUnexpressed
              deriving (Show,Eq,Ord,Enum,Bounded,Generic)

                       
