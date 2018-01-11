{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module WikiEL.Type where

import           Control.Lens              (makeLenses,makePrisms)
import           Data.Aeson
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Text                 (Text)
import qualified Data.Text           as T
import           Data.Vector               (Vector,toList)
import qualified Data.Vector.Unboxed as UV
import           GHC.Generics              (Generic)
--
import qualified Graph               as G
import qualified Graph.ETL           as G.E
import qualified Graph.Internal.Hash as H
import           Graph.Internal.Hash       (WordHash)
import           NLP.Type.NamedEntity      (NamedEntityClass)
--
import           WikiEL.Type.Wikidata      (ItemID)

data EntityToken = EntityToken { _word :: Text
                               , _tag  :: Text
                               } deriving (Show)

makeLenses ''EntityToken

data IRange = IRange { _beg :: Int
                     , _end :: Int}
                deriving(Eq,Generic)

makeLenses ''IRange

instance ToJSON IRange where
  toJSON = genericToJSON defaultOptions

instance FromJSON IRange where
  parseJSON = genericParseJSON defaultOptions

instance Show IRange where
  show (IRange b e) = "IRange [" ++ show b ++ "," ++ show e ++ ")"

data RelativePosition = LbeforeR | RbeforeL | Coincide | RinL | LinR | LoverlapR | RoverlapL
                      deriving(Show,Eq)

makePrisms ''RelativePosition

data ItemClass = ItemClass { _itemID  :: ItemID
                           , _strName :: Text }
                  deriving (Eq,Ord,Generic)

makeLenses ''ItemClass

instance Show ItemClass where
  show i = "Class:" ++ show (_strName i)

instance ToJSON ItemClass where
  toJSON = genericToJSON defaultOptions

instance FromJSON ItemClass where
  parseJSON = genericParseJSON defaultOptions

data TextMatchedEntityType = PublicCompany | PrivateCompany deriving (Show,Eq,Generic)

instance ToJSON TextMatchedEntityType
instance FromJSON TextMatchedEntityType

data PreNE = UnresolvedUID NamedEntityClass           -- Tagged by CoreNLP NER, but no matched Wikidata UID                
           | AmbiguousUID ([ItemID],NamedEntityClass) -- Tagged by CoreNLP NER, and matched Wikidata UIDs of the NamedEntityClass
           | Resolved (ItemID, ItemClass)             -- A wikidata UID of a CoreNLP NER Class type.                                
           | UnresolvedClass [ItemID]                 -- Not tagged by CoreNLP NER, but matched Wikidata UID(s)                     
           | OnlyTextMatched ItemID TextMatchedEntityType
           deriving(Show, Eq, Generic)

makePrisms ''PreNE

instance ToJSON PreNE where
  toJSON = genericToJSON defaultOptions

instance FromJSON PreNE where
  parseJSON = genericParseJSON defaultOptions

newtype EntityMentionUID = EntityMentionUID { _emuid :: Int} deriving (Eq,Generic)

makeLenses ''EntityMentionUID

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

-- Duplicated function
entityName :: EMInfo Text -> Text
entityName (_, ws, _) = T.intercalate " " (toList ws)

toString :: EMInfo Text -> String
toString em@(range, _ws, t) = show range ++ " \"" ++ T.unpack (entityName em) ++  "\", " ++show t

instance (Show a) => Show (UIDCite a (EMInfo Text))  where
  show (Cite uid ref info) = "Cite {" ++ show uid ++ " cites " ++ show ref ++ ",\t" ++ toString info ++ "}"
  show (Self uid info) = "Self {" ++ show uid  ++ ",\t" ++ toString info ++ "}"

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
                                 , _names :: Vector WordsHash
                                 } deriving (Show)

makeLenses ''NameUIDTable

type SortedEdges = (G.Direction, UV.Vector (H.WordHash, H.WordHash))
type NodeNames   = M.Map H.WordHash G.E.BString

data SortedGraph = SortedGraph SortedEdges NodeNames

data WikiuidNETag = WikiuidNETag { _set :: S.Set (ItemID, ItemClass)
                                 } deriving (Show)

makeLenses ''WikiuidNETag

