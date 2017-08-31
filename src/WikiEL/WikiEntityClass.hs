{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module WikiEL.WikiEntityClass where

import           Data.Aeson
import           Data.Text                             (Text)
import           Data.Map                              (Map)
import           Data.Maybe                            (fromMaybe,fromJust)
import           Data.List                             (foldl')
import           Control.Arrow                         (second)
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
brandClass  = buildItemClass "Q431289"
locationClass  = buildItemClass "Q1496967" -- territorial entity (Q1496967), instead of location (Q17334923)
occupationClass = buildItemClass "Q12737077"
humanRuleClass = buildItemClass "Q1151067"
buildingClass  = buildItemClass "Q41176"

toNEClass :: ItemClass -> NEClass
toNEClass c | c==orgClass      = N.Org
toNEClass c | c==personClass   = N.Person
toNEClass c | c==locationClass = N.Loc
toNEClass _ = N.Other

neClassMatch :: NEClass -> ItemClass -> Bool
neClassMatch N.Org    ic | ic==orgClass      = True
neClassMatch N.Person ic | ic==personClass   = True
neClassMatch N.Loc    ic | ic==locationClass = True
neClassMatch N.Other  ic | ic==brandClass || ic==occupationClass = True
neClassMatch _ _ = False


loadTypedUIDs :: (ItemClass , ItemIDFile) -> IO [(ItemID, ItemClass)]
loadTypedUIDs (tag, fileName) = do
  items <- loadItemIDs fileName
  let 
    uids = map (\x -> (x, tag)) items
  return uids

data WikiUID2NETag = WikiUID2NETag { _map :: Map ItemID ItemClass}
                   deriving (Show)

fromList :: [(ItemID, ItemClass)] -> WikiUID2NETag
fromList pairs = WikiUID2NETag (M.fromList pairs)

fromFiles :: [(ItemClass, ItemIDFile)] -> IO WikiUID2NETag
fromFiles pairs = do
  lists <- mapM loadTypedUIDs pairs
  let
    table = fromList (mconcat lists)
  return table



getNEClass :: WikiUID2NETag -> ItemID -> ItemClass
getNEClass table uid = f (M.lookup uid (_map table))
  where 
    f (Just x) = x
    f Nothing  = otherClass -- for unknown UID, return "Other" class, instead of emitting errors


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
    unique = S.toList . S.fromList
    allUIDs = unique (concatMap (\(SubclassUID x, SuperclassUID y) -> [x,y]) relTuples)
    relations = buildRelations relTuples
    f uid = map (\x -> (SubclassUID uid, SuperclassUID x)) (getAncestors relations uid)
    pairs = S.fromList (concatMap f allUIDs)

isSubclass :: S.Set (SubclassUID, SuperclassUID) -> SuperclassUID -> SubclassUID -> Bool
isSubclass pairs super sub = S.member (sub, super) pairs

