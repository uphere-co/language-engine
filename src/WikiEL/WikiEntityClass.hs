{-# LANGUAGE OverloadedStrings #-}

module WikiEL.WikiEntityClass where

import           Data.Text                             (Text)
import           Data.Map                              (Map)
import           Data.Maybe                            (fromMaybe,fromJust)
import           Data.List                             (foldl')
import           Control.Arrow                         (second)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           NLP.Type.NamedEntity                  (NamedEntityClass)
import           WikiEL.Type.Wikidata                 (ItemID)
import           WikiEL.Type.FileFormat               
import           WikiEL.ETL.LoadData                   


type NEClass = NamedEntityClass

loadTypedUIDs :: (NEClass , ItemIDFile) -> IO [(ItemID, NEClass)]
loadTypedUIDs (tag, fileName) = do
  items <- loadItemIDs fileName
  let 
    uids = map (\x -> (x, tag)) items
  return uids

data WikiUID2NETag = WikiUID2NETag { _map :: Map ItemID NEClass}
                   deriving (Show)

fromList :: [(ItemID, NEClass)] -> WikiUID2NETag
fromList pairs = WikiUID2NETag (M.fromList pairs)

fromFiles :: [(NEClass, ItemIDFile)] -> IO WikiUID2NETag
fromFiles pairs = do
  lists <- mapM loadTypedUIDs pairs
  let
    table = fromList (mconcat lists)
  return table



getNEClass :: WikiUID2NETag -> ItemID -> NEClass
getNEClass table uid = f (M.lookup uid (_map table))
  where 
    f (Just x) = x
    f _ = error ("Unknown UID: " ++ show uid)


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
