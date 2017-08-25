{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.Type.CoreNLP where

import           Control.Lens
import           Data.Aeson
import           Data.Binary                   (Binary)
import           Data.Function                 (on)
import qualified Data.Graph              as G
import qualified Data.IntMap             as IM
import           Data.List                     (sortBy)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Data.Tree
import           GHC.Generics
--
import           NLP.Type.NamedEntity
import           NLP.Type.PennTreebankII
import qualified NLP.Type.UniversalDependencies2.Syntax as U


data SentenceIndex = SentenceIndex { _sent_index      :: Int
                                   , _sent_charRange  :: (Int,Int)
                                   , _sent_tokenRange :: (Int,Int)
                                   } 
                   deriving (Generic, Show)

makeLenses ''SentenceIndex

instance ToJSON SentenceIndex where
  toJSON = genericToJSON defaultOptions

instance FromJSON SentenceIndex where
  parseJSON = genericParseJSON defaultOptions

instance Binary SentenceIndex


data Token = Token { _token_tok_idx_range :: (Int,Int)
                   , _token_char_idx_range :: (Int,Int)
                   , _token_text :: Text
                   , _token_pos :: POSTag
                   , _token_lemma :: Text }
           deriving (Generic, Show)

makeLenses ''Token

instance ToJSON Token where
  toJSON = genericToJSON defaultOptions

instance FromJSON Token where
  parseJSON = genericParseJSON defaultOptions

instance Binary Token


type Node = (Int,Text)

type Edge = ((Int,Int),U.DependencyRelation)


data Dependency = Dependency Int [Node] [Edge]
                deriving (Show,Eq,Ord,Generic)

instance ToJSON Dependency where
  toJSON = genericToJSON defaultOptions

instance FromJSON Dependency where
  parseJSON = genericParseJSON defaultOptions

instance Binary Dependency


data Sentence = Sentence { _sentenceLemma :: [Text]
                         , _sentenceToken :: [Maybe Token]
                         , _sentenceWord  :: [Maybe Text]
                         , _sentenceNER   :: [Maybe Text]
                         } deriving (Generic, Show)

makeLenses ''Sentence

instance ToJSON Sentence where
  toJSON = genericToJSON defaultOptions

instance FromJSON Sentence where
  parseJSON = genericParseJSON defaultOptions

instance Binary Sentence

dependencyIndexTree :: Dependency -> Tree G.Vertex
dependencyIndexTree (Dependency root nods edgs0) =
  let bnds = let xs = map fst nods in (minimum xs, maximum xs)
      edgs = map fst edgs0
  in head (G.dfs (G.buildG bnds edgs) [root])


dependencyLabeledTree :: Dependency -> Tree (G.Vertex,U.DependencyRelation)
dependencyLabeledTree dep@(Dependency root nods edgs0) =
  let tr = dependencyIndexTree dep
      emap = IM.fromList (map (\((_,i),rel) -> (i,rel)) edgs0)
  in normalizeOrder (fmap (\i -> (i,fromMaybe U.ROOT (IM.lookup i emap))) tr)


normalizeOrder :: Tree (G.Vertex,U.DependencyRelation) -> Tree (G.Vertex,U.DependencyRelation)
normalizeOrder (Node x xs) = let xs' = map normalizeOrder xs in Node x (sortBy (compare `on` ((^._1).rootLabel)) xs')


type NERToken = (Text,NamedEntityClass)

newtype NERSentence = NERSentence [NERToken]
                    deriving (Show,Eq)

