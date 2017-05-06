{-# LANGUAGE TemplateHaskell #-}

module HFrameNet.Type where

import           Control.Lens
import           Data.Text      (Text)
import qualified Data.Text as T

type DateTime = Text

data Frame = Frame { _frame_ID :: Int
                   , _frame_name :: Text
                   , _frame_cDate :: DateTime
                   , _frame_definition :: Text
                   -- , _frame_fe :: [FE]
                   -- , _frame_lexUnit :: [LexUnit]
                   }
           deriving (Show)

makeLenses ''Frame

{- 
data FE = FE { _fe_ID :: Int
             , _fe_name :: Text
             , _fe_abbrev :: Text
             , _fe_cDate :: DateTime
             , _fe_coreType :: Text
             , _fe_fgColor :: Text
             , _fe_bgColor :: Text
             , _fe_definition :: Text
             , _fe_semType :: [Sem]
             }

makeLenses ''FE
-}  
