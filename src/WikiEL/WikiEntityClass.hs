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
import qualified WikiEL.WikiEntity             as Wiki


type NEClass = NamedEntityClass

loadTypedUIDs :: (NEClass , FilePath) -> IO [(Wiki.UID, NEClass)]
loadTypedUIDs (tag, fileName) = do
  content <- T.IO.readFile fileName
  let 
    uids = map (\x -> (Wiki.UID x, tag)) (T.lines content)
  return uids

data WikiUID2NETag = WikiUID2NETag { _map :: Map Wiki.UID NEClass}
                   deriving (Show)

fromFiles :: [(NEClass, FilePath)] -> IO WikiUID2NETag
fromFiles pairs = do
  lists <- mapM loadTypedUIDs pairs
  let
    table = WikiUID2NETag (M.fromList (mconcat lists))
  return table

fromList :: [(Wiki.UID, NEClass)] -> WikiUID2NETag
fromList pairs = WikiUID2NETag (M.fromList pairs)


getNEClass :: WikiUID2NETag -> Wiki.UID -> NEClass
getNEClass table uid = f (M.lookup uid (_map table))
  where 
    f (Just x) = x
    f _ = error ("Unknown UID: " ++ T.unpack (Wiki._uid uid))


newtype SubclassUID   = SubclassUID { _sub :: Wiki.UID}
                      deriving (Show, Ord, Eq)
newtype SuperclassUID   = SuperclassUID { _super :: Wiki.UID}
                      deriving (Show, Ord, Eq)

type SuperClasses = M.Map SubclassUID [Wiki.UID]

buildRelations :: [(SubclassUID, SuperclassUID)] -> SuperClasses
buildRelations relations = M.fromListWith (++) (map (second (\(SuperclassUID x) -> [x])) relations)

getAncestors :: SuperClasses -> Wiki.UID -> [Wiki.UID]
getAncestors map key = g key (lookups map key)
  where
    lookups map key = fromMaybe [] (M.lookup (SubclassUID key) map)
    g key [] = [key]
    g key vals = key : concatMap (\v -> g v (lookups map v)) vals
        

parseRelationLine :: Text -> (SubclassUID, SuperclassUID)
parseRelationLine line = (SubclassUID (Wiki.UID sub), SuperclassUID (Wiki.UID super))
  where
    [sub, subStr, super, superStr] = T.splitOn "\t" line

getKeys :: SuperClasses -> [Wiki.UID]
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
