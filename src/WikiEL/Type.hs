{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module WikiEL.Type where

import           Control.Lens         (makePrisms)
import           Data.Aeson
import           Data.Text            (Text)
import qualified Data.Text    as T
import           Data.Vector          (Vector)
import qualified Data.Vector.Unboxed as UV
import           GHC.Generics         (Generic)
--
import           Graph.Internal.Hash  (WordHash)
import           NLP.Type.NamedEntity (NamedEntityClass)
--
import           WikiEL.Type.Wikidata (ItemID)

data EntityToken = EntityToken { word :: Text
                               , tag  :: Text
                               } deriving (Show)

data IRange = IRange { beg :: Int
                     , end :: Int}
                deriving(Eq,Generic)

instance ToJSON IRange where
  toJSON = genericToJSON defaultOptions

instance FromJSON IRange where
  parseJSON = genericParseJSON defaultOptions

instance Show IRange where
  show (IRange beg end) = "IRange [" ++ show beg ++ "," ++ show end ++ ")"

data RelativePosition = LbeforeR | RbeforeL | Coincide | RinL | LinR | LoverlapR | RoverlapL
                      deriving(Show,Eq)

parseNERToken :: Text -> EntityToken
parseNERToken tokenStr = (\(x,y)-> EntityToken (T.dropEnd 1 x) y) $ T.breakOnEnd (T.pack "/") tokenStr

parseNEROutputStr :: Text -> [EntityToken]
parseNEROutputStr str = map parseNERToken (T.words str)

data ItemClass = ItemClass { _itemID  :: ItemID
                           , _strName :: Text }
                  deriving (Eq,Ord,Generic)

instance Show ItemClass where
  show id = "Class:" ++ show (_strName id)

instance ToJSON ItemClass where
  toJSON = genericToJSON defaultOptions

instance FromJSON ItemClass where
  parseJSON = genericParseJSON defaultOptions


data PreNE = UnresolvedUID NamedEntityClass             -- Tagged by CoreNLP NER, but no matched Wikidata UID                
           | AmbiguousUID ([ItemID],NamedEntityClass)   -- Tagged by CoreNLP NER, and matched Wikidata UIDs of the NamedEntityClass                                                                                                                      
           | Resolved (ItemID, ItemClass)  -- A wikidata UID of a CoreNLP NER Class type.                                
           | UnresolvedClass [ItemID]          -- Not tagged by CoreNLP NER, but matched Wikidata UID(s)                     
           deriving(Show, Eq, Generic)

makePrisms ''PreNE

instance ToJSON PreNE where
  toJSON = genericToJSON defaultOptions

instance FromJSON PreNE where
  parseJSON = genericParseJSON defaultOptions


newtype EntityMentionUID = EntityMentionUID { _emuid :: Int} deriving (Generic)

instance Show EntityMentionUID where
  show (EntityMentionUID uid) = "EMuid " ++ show uid

{-|
  Self - entity mention; e.g. Michael Jordan
  Cite - a reference to an entity mention; e.g. Jordan
-}
data UIDCite uid info = Cite { _uid  :: uid
                             , _ref  :: uid
                             , _info :: info} 
                      | Self { _uid  :: uid
                             , _info :: info}
                      deriving(Eq,Generic)

makePrisms ''UIDCite

-- w : type of word token
type EMInfo w = (IRange, Vector w, PreNE)
type EntityMention w = UIDCite EntityMentionUID (EMInfo w)

instance ToJSON EntityMentionUID where
  toJSON = genericToJSON defaultOptions

instance FromJSON EntityMentionUID where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON (EntityMention Text) where
  toJSON = genericToJSON defaultOptions

instance FromJSON (EntityMention Text) where
  parseJSON = genericParseJSON defaultOptions

type WordsHash = UV.Vector WordHash

data NameUIDTable = NameUIDTable { _uids :: Vector ItemID
                                 , _names :: Vector WordsHash}
                  deriving (Show)
