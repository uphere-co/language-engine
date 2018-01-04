{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.Semantics.Type where

import           Control.Lens (makeLenses)
import           Data.Aeson
import           Data.Text
import           GHC.Generics

data PrepOr a = PrepOr { _po_prep :: Maybe Text
                       , _po_main :: a
                       }
              deriving (Eq,Show,Generic)

makeLenses ''PrepOr

instance ToJSON a => ToJSON (PrepOr a)
instance FromJSON a => FromJSON (PrepOr a)

noprep = PrepOr Nothing

type Frame = Text
type FrameElement = Text
type TWord = Text


--
-- | MeaningTree is a representation of meaning. This is necessary in web interface because original
--   meaning graph is too lengthy to be appeared in webpage. It consists of frame, predicate and
--   arguments. Each argument forms a pair of (frame element,word).
--
data MeaningTree = MeaningTree
  { _mt_vertexID :: Int
  , _mt_frame:: Frame       -- ^ Unlemma form of verb when predicate. Frame when nominal predicate.
  , _mt_predicate :: TWord      -- ^ Predicate word.
  , _mt_isNegated :: Bool       -- ^ Negation information of verb.
  , _mt_arguments :: [(Int,FrameElement,Either (PrepOr MeaningTree) (PrepOr (TWord,Maybe Int)))]
    -- ^ A tuple of FrameElement and word, or a tuple of FrameElement and subsequent ARB.
  } deriving (Eq,Show, Generic)

makeLenses ''MeaningTree

instance ToJSON MeaningTree
instance FromJSON MeaningTree
