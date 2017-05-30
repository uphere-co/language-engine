{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EntityLinking where

import           Data.List                             (inits,foldl')
import           Data.Vector                           (Vector)
import qualified Data.Text                  as T
import           Data.Text                             (Text)

import           Misc                                  (IRange(..),RelativePosition(..),relativePos,isContain,subVector) 
import           NamedEntity                           (NamedEntity, OrderedNamedEntity)
import           WikiNamedEntityTagger                 (PreNE(..))
import qualified NamedEntity                as N

mayRefer :: NamedEntity -> NamedEntity -> Bool
mayRefer src target = (N._type src == N._type target) && T.isInfixOf (N._str src) (N._str target)

canRefer :: OrderedNamedEntity -> OrderedNamedEntity -> Bool
canRefer src target = (N._order src > N._order target) && mayRefer (N._entity src) (N._entity target)


newtype EntityMentionUID = EntityMentionUID { _emuid :: Int}

instance Show EntityMentionUID where
  show (EntityMentionUID uid) = "EMuid " ++ show uid

data UIDCite uid info = Cite { _uid  :: uid
                             , _ref  :: uid
                             , _info :: info} 
                      | Self { _uid  :: uid
                             , _info :: info}
                      deriving(Show, Eq)

-- w : type of word token
type EMInfo w = (IRange, Vector w, PreNE)
type EntityMention w = UIDCite EntityMentionUID (EMInfo w)

buildEntityMentions :: Vector w -> [(IRange, PreNE)] -> [EntityMention w]
buildEntityMentions text wikiNEs = zipWith Self uids mentions
  where
    uids     = map EntityMentionUID [1..]
    mentions = map (\(range,e) -> (range, subVector range text, e)) wikiNEs


tryEntityLink :: Eq a => EMInfo a -> EMInfo a -> Maybe (EMInfo a)
tryEntityLink target@(trange, twords, ttag) src@(srange, swords, stag) =
  f (relativePos trange srange) (isContain swords twords) ttag stag
  where 
    f pos textMatch (Resolved (uid,ttag)) (UnresolvedUID stag) | pos==LbeforeR && textMatch && ttag==stag =
      Just (srange,swords, Resolved (uid,ttag))
    f _ _ _ _ = Nothing

entityLinking :: Eq a => [EntityMention a] -> EntityMention a -> EntityMention a
entityLinking targets src = foldr f src targets
  where
    f target src@(Self idx info)       =
      case tryEntityLink (_info target) info of
        Nothing   -> src
        Just linked -> Cite idx (_uid target) linked
    f _      src = src

entityLinkings :: Eq a => [EntityMention a] -> [EntityMention a]
entityLinkings srcs = reverse (foldl' f [] srcs)
  where
    f targets src = entityLinking targets src : targets


