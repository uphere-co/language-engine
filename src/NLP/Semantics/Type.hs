{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.Semantics.Type where

import           Control.Lens (makeLenses,makePrisms)
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
type TWord = (Text,Maybe (Int,Int))


data MeaningRoleContent = SubFrame MeaningTree
                        | Modifier TWord [MeaningTree]
                        | Terminal TWord (Maybe (Bool,Int))
                        deriving (Eq,Show,Generic)

instance ToJSON MeaningRoleContent
instance FromJSON MeaningRoleContent


--
-- | MeaningRole is a single instance of each Frame element to content matching.
--   Content can be either a final sequence of words or a subframe represented by MeaningTree.
--
data MeaningRole = MeaningRole { _mr_id :: Int
                               , _mr_role :: FrameElement
                               , _mr_content :: PrepOr MeaningRoleContent
                               }
                 deriving (Eq,Show,Generic)

instance ToJSON MeaningRole
instance FromJSON MeaningRole

--
-- | MeaningTree is a representation of meaning. This is necessary in web interface because original
--   meaning graph is too lengthy to be appeared in webpage. It consists of frame, predicate and
--   arguments. Each argument forms a pair of (frame element,word).
--
data MeaningTree = MeaningTree
  { _mt_vertexID :: Int
  , _mt_frame:: Frame              -- ^ Unlemma form of verb when predicate. Frame when nominal predicate.
  , _mt_predicate :: TWord         -- ^ Predicate word.
  , _mt_isNegated :: Bool          -- ^ Negation information of verb.
  , _mt_arguments :: [MeaningRole] -- ^ A list of MeaningRole's
  , _mt_subordinates :: [MeaningTree]
  } deriving (Eq,Show, Generic)


instance ToJSON MeaningTree
instance FromJSON MeaningTree

makePrisms ''MeaningRoleContent
makeLenses ''MeaningRole
makeLenses ''MeaningTree

-- | ARB is the representation of meaning graph with NER tagging. This is necessary in web interface because original meaning graph is too lengthy to be appeared
--   in webpage. It consists of subject, predicate, object and hashtags. Each item is appeared as a pair of word and its semantic role like Frame or FrameElement.
--   Hashtag will be used to show additional information, but it is not used now. Object can be an (noun) entity or another subsequent ARB, though only an entity
--   can be subject.
data ARB = ARB
  { _frameARB :: Frame                -- ^ Unlemma form of verb when predicate. Frame when nominal predicate.
  , _subjectA :: (FrameElement,Text)  -- ^ FrameElement of subject in a sentence and word itself.
  , _predicateR :: (Frame,Text)      -- ^ FrameElement of predicate and word itself. Frame is a duplication to _frameARB.
  , _isNegated :: Bool                -- ^ Negation information of verb.
  , _objectB :: [(FrameElement,Either (PrepOr ARB) (PrepOr Text))] -- ^ A tuple of FrameElement and word, or a tuple of FrameElement and subsequent ARB.
  , _hashTags :: [Text] -- ^ A list of words to be presented as hashtags.
  } deriving (Eq,Show, Generic)

makeLenses ''ARB

instance ToJSON ARB
instance FromJSON ARB
