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


buildItemClass :: Text -> Text -> ItemClass
buildItemClass x name = ItemClass (itemID x) name

{-|
  A list of constant values for Wikipedia entity classes
-}
otherClass,orgClass,personClass,locationClass,brandClass,occupationClass,humanRuleClass,buildingClass :: ItemClass
otherClass  = buildItemClass "Q35120"  "Other"-- maps to entity (Q35120), which means "anything"
orgClass    = buildItemClass "Q43229"  "Organization"
personClass = buildItemClass "Q215627" "Person"
locationClass   = buildItemClass "Q1496967"  "Location"-- territorial entity (Q1496967), instead of location (Q17334923)
brandClass      = buildItemClass "Q431289"   "Brand"
occupationClass = buildItemClass "Q12737077" "Occupation"
humanRuleClass  = buildItemClass "Q1151067"  "HumanRule"
buildingClass   = buildItemClass "Q41176"    "Building"


extendedClasses :: [ItemClass]
extendedClasses = [brandClass,occupationClass,humanRuleClass,buildingClass]


allClasses :: [ItemClass]
allClasses      = [otherClass, orgClass, personClass, locationClass] ++ extendedClasses


fromNEClass :: NamedEntityClass -> ItemClass
fromNEClass N.Org    = orgClass
fromNEClass N.Person = personClass
fromNEClass N.Loc    = locationClass
fromNEClass _        = otherClass

{-| 
  Since CoreNLP NER classes, NEClass, are more coarse, we need inclusion, or a sort of subclass, relationship 
  between ItemClass and NEClass.
-}
mayCite :: NamedEntityClass -> ItemClass -> Bool 
mayCite N.Org    c | c==orgClass      = True
mayCite N.Person c | c==personClass   = True
mayCite N.Loc    c | c==locationClass = True
mayCite N.Date   c | c==humanRuleClass= True
mayCite N.Time   c | c==humanRuleClass= True
mayCite N.Money  c | c==humanRuleClass= True
mayCite N.Other  _             = True
mayCite N.Misc   _             = True
mayCite _        _             = False

loadTypedUIDs :: (ItemClass , ItemIDFile) -> IO [(ItemID, ItemClass)]
loadTypedUIDs (tag, fileName) = do
  items <- loadItemIDs fileName
  let 
    uids = map (\x -> (x, tag)) items
  return uids

loadFiles :: [(ItemClass, ItemIDFile)] -> IO WikiuidNETag
loadFiles pairs = do
  lists <- mapM loadTypedUIDs pairs
  return $ WikiuidNETag (S.fromList (mconcat lists))

fromList :: [(ItemID, ItemClass)] -> WikiuidNETag
fromList pairs = WikiuidNETag (S.fromList pairs)
  
hasNETag :: WikiuidNETag -> (ItemID, NamedEntityClass) -> Bool
hasNETag (WikiuidNETag tags) (i,stag) | stag /= N.Other = S.member (i,fromNEClass stag) tags
                                      | otherwise       = any (\x -> S.member (i,x) tags) extendedClasses
                                        
-- hasNETag (WikiuidNETag tags) (i,stag) = any (\x -> S.member (i,x) tags) extendedClasses

guessItemClass :: WikiuidNETag -> ItemID -> ItemClass
guessItemClass (WikiuidNETag tags) i = fromMaybe otherClass x
  where
    x = L.find (\y -> S.member (i,y) tags) allClasses

guessItemClass2 :: WikiuidNETag -> NamedEntityClass -> ItemID -> ItemClass
guessItemClass2 tags ne i | hasNETag tags (i,ne) = fromNEClass ne
                          | otherwise            = guessItemClass tags i
-- guessItemClass2 tags ne i = guessItemClass tags id
  
    
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
