{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module WikiEL.WikiEntityClass where

import           Data.Text                             (Text)
import           Data.Maybe                            (fromMaybe)
import           Data.List                             (any)
import           Control.Arrow                         (second)
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           NLP.Type.NamedEntity                  (NamedEntityClass)
import qualified NLP.Type.NamedEntity          as N
import           WikiEL.Type                           (ItemClass(..),WikiuidNETag(..))
import           WikiEL.Type.Wikidata                  (ItemID)
import           WikiEL.Type.FileFormat
import           WikiEL.ETL.LoadData
import           WikiEL.ETL.Parser











loadTypedUIDs :: (ItemClass , ItemIDFile) -> IO [(ItemID, ItemClass)]
loadTypedUIDs (tag, fileName) = do
  items <- loadItemIDs fileName
  let 
    uids = map (\x -> (x, tag)) items
  return uids

fromList :: [(ItemID, ItemClass)] -> WikiuidNETag
fromList pairs = WikiuidNETag (S.fromList pairs)
  
hasNETag :: WikiuidNETag -> (ItemID, NamedEntityClass) -> Bool
hasNETag (WikiuidNETag tags) (i,stag) | stag /= N.Other = S.member (i,fromNEClass stag) tags
                                      | otherwise       = any (\x -> S.member (i,x) tags) extendedClasses
                                        
-- hasNETag (WikiuidNETag tags) (i,stag) = any (\x -> S.member (i,x) tags) extendedClasses

    
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
getAncestors mp key = g key (lookups mp key)
  where
    lookups m k = fromMaybe [] (M.lookup (SubclassUID k) m)
    g k [] = [k]
    g k vals = k : concatMap (\v -> g v (lookups mp v)) vals
        

getKeys :: SuperClasses -> [ItemID]
getKeys = M.foldlWithKey' (\ks (SubclassUID k) _ -> k:ks) []

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
