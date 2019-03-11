{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module WikiEL.Type where

import           Control.Lens  ( (^.), makeLenses, makePrisms )
import           Data.Aeson    ( FromJSON(..), ToJSON(..), defaultOptions
                               , genericParseJSON, genericToJSON )
import           Data.Hashable ( Hashable )
import qualified Data.Set as S
import           Data.Text     ( Text )
import qualified Data.Text as T
import           Data.Vector   ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import GHC.Generics (Generic)
------ other language-engine
import           Graph.Internal.Hash  ( WordHash )
import           NLP.Type.CoreNLP     ( Sentence )
import           NLP.Type.NamedEntity ( NamedEntity, NamedEntityClass(..), OrderedNamedEntity )
import qualified NLP.Type.NamedEntity as N

-- old WikiEL.Type.Wikidata

{-|
  Each Wikidata entity has an unique ID, e.g. Q30 for USA.
  ItemID is for representing the ID.
  PropertyID is for Wikidata properties.
  ItemRepr is for a name or an alias of Wikidata entity.
-}

data ItemID = QID { _qitemID :: Int }
            | CID { _citemID :: Int }
            deriving (Eq,Ord,Generic)

makePrisms ''ItemID

instance Hashable ItemID

instance ToJSON ItemID where
  toJSON = genericToJSON defaultOptions

instance FromJSON ItemID where
  parseJSON = genericParseJSON defaultOptions

instance Show ItemID where
  show (QID i) = "Q" ++ show i
  show (CID i) = "C" ++ show i

newtype ItemRepr = ItemRepr { _repr :: Text}
                 deriving (Show, Eq, Ord)


newtype PropertyID = PropertyID { _propID :: Int }
               deriving (Eq, Ord)
instance Show PropertyID where
  show (PropertyID uid) = "P" ++ show uid

-- old WikiEL.Type

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
entityName (_, ws, _) = T.intercalate " " (V.toList ws)

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


data WikiuidNETag = WikiuidNETag { _set :: S.Set (ItemID, ItemClass)
                                 } deriving (Show)

makeLenses ''WikiuidNETag


newtype NETagger = NETagger { unNETagger :: [Sentence] -> [EntityMention Text] }

-- | Dummy NETagger if one wants to bypass NETagger
emptyNETagger :: NETagger
emptyNETagger = NETagger (const [])




-- old WikiEL.Type.FileFormat

data EntityReprRow = EntityReprRow { _row_uid  :: ItemID
                                   , _row_repr :: ItemRepr
                                   }
                    deriving (Show)

--
newtype EntityReprFile = EntityReprFile { unEntityReprFile :: FilePath }
                        deriving (Show)


--
newtype ItemIDFile = ItemIDFile { unItemIDFile :: FilePath }
                   deriving (Show)

type ItemIDRow = ItemID


data PropertyNameRow = PropertyNameRow { _prop     :: PropertyID
                                       , _propName :: Text
                                       }
                     deriving (Show)


data Synset = Synset { _synsetRepr :: Text
                     , _synsetPos  :: Text
                     , _synsetIdx  :: Int
                     }
            deriving (Eq,Show)

data SynsetY = SynsetY { _synsetYName :: Text
                       , _synsetYId  :: Int
                       }
            deriving (Eq,Show)

data WordNetMappingRow = WordNetMappingRow { _wordNetPageTitle :: Text
                                           , _wordNetPageID    :: PageID
                                           , _wordNetItemID    :: ItemID
                                           , _wordNetSynset    :: Synset
                                           }
                       deriving (Show)

type WikiTitleMappingRow = (ItemID, Text)



data SubclassRelationRow = SubclassRelationRow { _sub :: ItemID
                                               , _super :: ItemID
                                               }
                          deriving (Show,Eq)



-- old WikiEL.Type.Wikipedia



{-|
  Each Wikipedia entity has their own page with a unique title.
  Internally, each title has an unique page ID, too.
  This IDs are appeared in Wikipedia dumps.
  PageTitle is for representing Wikipedia title.
  PageID is for representing a page id of a title.
-}

newtype PageID = PageID {_id :: Int }
               deriving (Eq, Ord)

instance Show PageID where
  show (PageID uid) = "W" ++ show uid

newtype PageTitle = PageTitle {_title :: Int }
               deriving (Eq, Ord)

instance Show PageTitle where
  show (PageTitle title) = "enwiki/" ++ show title

-- old WikiEL.Type.Equity

newtype Symbol = Symbol { _symbol :: Text }
               deriving (Eq, Ord, Show)
newtype Exchange = Exchange { _exchange :: Text }
                 deriving (Eq, Ord, Show)
data EquityTicker = EquityTicker Exchange Symbol
                  deriving (Eq, Ord)

instance Show EquityTicker where
  show (EquityTicker e s) = show (_exchange e) ++ ":" ++ show (_symbol s)


newtype GICS = GICS { _gics :: Text }
             deriving (Eq, Ord)

instance Show GICS where
  show (GICS sector) = "GICS:" ++ show sector

newtype GICSsub = GICSsub { _gicsSub :: Text }
             deriving (Eq, Ord)

instance Show GICSsub where
  show (GICSsub sector) = "GICS_sub:" ++ show sector

-- old WikiEL.Convert

getNameFromEntityMention :: EntityMention Text -> Text
getNameFromEntityMention x =
  case x of
    Cite _uid _ref _info@(_ir,v,_ne) -> T.intercalate " " (V.toList v)
    Self _uid      _info@(_ir,v,_ne) -> T.intercalate " " (V.toList v)


getRangeFromEntityMention :: EntityMention Text -> (Int,Int)
getRangeFromEntityMention x =
  case x of
    Cite _uid _ref _info@(ir,_v,_ne) -> (ir ^. beg,ir ^. end)
    Self _uid      _info@(ir,_v,_ne) -> (ir ^. beg,ir ^. end)


getUIDFromEntityMention :: EntityMention Text -> Int
getUIDFromEntityMention x =
  case x of
    Cite uid _ref _info@(_ir,_v,_ne) -> (_emuid uid)
    Self uid      _info@(_ir,_v,_ne) -> (_emuid uid)


getNEFromEntityMention :: EntityMention Text -> Text
getNEFromEntityMention x =
  case x of
    Cite _uid _ref _info@(_ir,_v,ne) -> (T.pack $ show ne)
    Self _uid      _info@(_ir,_v,ne) -> (T.pack $ show ne)



-- old WikiEL.WikiNamedEntityTagger

uidCandidates :: PreNE -> [ItemID]
uidCandidates (UnresolvedUID _)      = []
uidCandidates (AmbiguousUID (ids,_)) = ids
uidCandidates (Resolved     (i,_))  = [i]
uidCandidates (UnresolvedClass ids)  = ids
uidCandidates (OnlyTextMatched i _)    = [i]

isResolved :: PreNE -> Bool
isResolved (Resolved _ ) = True
isResolved _ = False

resolvedUID :: PreNE -> Either String ItemID
resolvedUID (Resolved (i,_))     = Right i
resolvedUID (UnresolvedUID _)     = Left "Unresolved ItemID"
resolvedUID (AmbiguousUID _)      = Left "Ambiguous ItemID"
resolvedUID (UnresolvedClass _)   = Left "Unresolved named entity class"
resolvedUID (OnlyTextMatched i _) = Right i


-- old WikiEL.EntityLinking

mayRefer :: NamedEntity -> NamedEntity -> Bool
mayRefer src target = (N._type src == N._type target) && T.isInfixOf (N._str src) (N._str target)

canRefer :: OrderedNamedEntity -> OrderedNamedEntity -> Bool
canRefer src target = (N._order src > N._order target) && mayRefer (N._entity src) (N._entity target)


entityUID :: EntityMention w -> Either String ItemID
entityUID m = resolvedUID tag
  where
    (_,_,tag) = _info m

mentionedEntityName :: EntityMention Text -> Text
mentionedEntityName em = entityName (_info em)

entityUIDcandidates :: EntityMention w -> [ItemID]
entityUIDcandidates m = uidCandidates tag
  where
    (_,_,tag) = _info m


entityPreNE :: EntityMention a -> PreNE
entityPreNE mention = ne
  where (_,_, ne) = _info mention

hasResolvedUID :: EntityMention a -> Bool
hasResolvedUID mention  = isResolved (entityPreNE mention)
