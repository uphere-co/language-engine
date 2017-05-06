{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type where

import           Control.Lens
import           Data.Text      (Text)
import qualified Data.Text as T

type DateTime = Text

data SemType = SemType { _semtype_ID :: Int
                       , _semtype_name :: Text }
             deriving (Show)

makeLenses ''SemType

data FE = FE { _fe_ID :: Int
             , _fe_name :: Text
             , _fe_abbrev :: Text
             , _fe_cDate :: DateTime
             , _fe_coreType :: Text
             , _fe_fgColor :: Text
             , _fe_bgColor :: Text
             , _fe_definition :: Text
             , _fe_semType :: [SemType]
             }
          deriving (Show)

makeLenses ''FE

data Lexeme = Lexeme { _lexeme_name :: Text
                     , _lexeme_POS :: Text
                     , _lexeme_breakBefore :: Bool
                     , _lexeme_headword :: Bool
                     , _lexeme_order :: Int }
            deriving (Show)

makeLenses ''Lexeme

data LexUnit = LexUnit { _lexunit_ID :: Int
                       , _lexunit_name :: Text
                       , _lexunit_POS :: Text
                       , _lexunit_status :: Text
                       , _lexunit_cDate :: DateTime
                       , _lexunit_lemmaID :: Int
                       , _lexunit_definition :: Text
                       -- , _lexunit_annotation :: Annotation
                       , _lexunit_lexeme :: [Lexeme]
                       , _lexunit_semType :: [SemType]
                                            
                       }
               deriving (Show)

makeLenses ''LexUnit


data Frame = Frame { _frame_ID :: Int
                   , _frame_name :: Text
                   , _frame_cDate :: DateTime
                   , _frame_definition :: Text
                   , _frame_fe :: [FE]
                   , _frame_lexUnit :: [LexUnit]
                   }
           deriving (Show)

makeLenses ''Frame


