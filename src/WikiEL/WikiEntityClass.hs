{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module WikiEL.WikiEntityClass where

import           Data.Aeson
import           Data.Text                             (Text)
import           Data.Map                              (Map)
import           Data.Maybe                            (fromMaybe,fromJust)
import           Data.List                             (any,foldl')
import           Control.Arrow                         (second)
import qualified Data.List                     as L
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           GHC.Generics                          (Generic)

import           NLP.Type.NamedEntity                  (NamedEntityClass)
import qualified NLP.Type.NamedEntity          as N
import           WikiEL.Type.Wikidata                 (ItemID)
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.LoadData
import           WikiEL.ETL.Parser

type NEClass = NamedEntityClass

newtype ItemClass = ItemClass { _itemID :: ItemID }
                  deriving (Eq,Ord,Generic)

instance Show ItemClass where
  show (ItemClass id) = "Class:" ++ show id
                  
instance ToJSON ItemClass where
  toJSON = genericToJSON defaultOptions

instance FromJSON ItemClass where
  parseJSON = genericParseJSON defaultOptions

buildItemClass :: Text -> ItemClass
buildItemClass x = ItemClass (itemID x)

otherClass  = buildItemClass "Q35120" -- maps to entity (Q35120), which means "anything"
orgClass    = buildItemClass "Q43229"
personClass = buildItemClass "Q215627"
locationClass   = buildItemClass "Q1496967" -- territorial entity (Q1496967), instead of location (Q17334923)
brandClass      = buildItemClass "Q431289"
occupationClass = buildItemClass "Q12737077"
humanRuleClass  = buildItemClass "Q1151067"
buildingClass   = buildItemClass "Q41176"
extednedClasses = [brandClass,occupationClass,humanRuleClass,buildingClass]
allClasses      = [otherClass, orgClass, personClass, locationClass] ++ extednedClasses

toNEClass :: ItemClass -> NEClass
toNEClass c | c==orgClass      = N.Org
toNEClass c | c==personClass   = N.Person
toNEClass c | c==locationClass = N.Loc
toNEClass _ = N.Other

fromNEClass :: NEClass -> ItemClass
fromNEClass N.Org    = orgClass
fromNEClass N.Person = personClass
fromNEClass N.Loc    = locationClass
fromNEClass _        = otherClass


loadTypedUIDs :: (ItemClass , ItemIDFile) -> IO [(ItemID, ItemClass)]
loadTypedUIDs (tag, fileName) = do
  items <- loadItemIDs fileName
  let 
    uids = map (\x -> (x, tag)) items
  return uids

data WikiuidNETag = WikiuidNETag { _set :: S.Set (ItemID, ItemClass)}
                   deriving (Show)

loadFiles :: [(ItemClass, ItemIDFile)] -> IO WikiuidNETag
loadFiles pairs = do
  lists <- mapM loadTypedUIDs pairs
  return $ WikiuidNETag (S.fromList (mconcat lists))

fromList :: [(ItemID, ItemClass)] -> WikiuidNETag
fromList pairs = WikiuidNETag (S.fromList pairs)
  
hasNETag :: WikiuidNETag -> (ItemID, NEClass) -> Bool
hasNETag (WikiuidNETag tags) (id,stag) | stag /= N.Other = S.member (id,fromNEClass stag) tags
hasNETag (WikiuidNETag tags) (id,stag) = any (\x -> S.member (id,x) tags) extednedClasses

guessNETag :: WikiuidNETag -> ItemID -> ItemClass
guessNETag (WikiuidNETag tags) id = fromMaybe otherClass x
  where
    x = L.find (\x -> S.member (id,x) tags) allClasses

newtype SubclassUID   = SubclassUID { _sub :: ItemID}
                      deriving (Show, Ord, Eq)
newtype SuperclassUID   = SuperclassUID { _super :: ItemID}
                      deriving (Show, Ord, Eq)

type SuperClasses = M.Map SubclassUID [ItemID]

fromRows :: [SubclassRelationRow] -> [(SubclassUID, SuperclassUID)]
fromRows = map (\(SubclassRelationRow sub super) -> (SubclassUID sub, SuperclassUID super))

buildRelations :: [(SubclassUID, SuperclassUID)] -> SuperClasses
buildRelations relations = M.fromListWith (++) (map (second (\(SuperclassUID x) -> [x])) relations)

getAncestors :: SuperClasses -> ItemID -> [ItemID]
getAncestors map key = g key (lookups map key)
  where
    lookups map key = fromMaybe [] (M.lookup (SubclassUID key) map)
    g key [] = [key]
    g key vals = key : concatMap (\v -> g v (lookups map v)) vals
        

getKeys :: SuperClasses -> [ItemID]
getKeys = M.foldlWithKey' (\ks (SubclassUID k) x -> k:ks) []

allRelationPairs :: [(SubclassUID, SuperclassUID)] -> S.Set (SubclassUID, SuperclassUID)
allRelationPairs relTuples = pairs
  where
    unique  = S.toList . S.fromList
    allUIDs = unique (concatMap (\(SubclassUID x, SuperclassUID y) -> [x,y]) relTuples)
    relations = buildRelations relTuples
    f uid = map (\x -> (SubclassUID uid, SuperclassUID x)) (getAncestors relations uid)
    pairs = S.fromList (concatMap f allUIDs)

isSubclass :: S.Set (SubclassUID, SuperclassUID) -> SuperclassUID -> SubclassUID -> Bool
isSubclass pairs super sub = S.member (sub, super) pairs

