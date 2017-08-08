{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WikiEL.EntityLinking where

import           Data.List                             (inits,foldl')
import           Data.Vector                           (Vector,toList)
import           Data.Text                             (Text)
import qualified Data.Text                  as T

import           WikiEL.Misc                           (IRange(..),RelativePosition(..),relativePos,isContain,subVector) 
import           WikiEL.Type.Wikidata                  (ItemID)
import           NLP.Type.NamedEntity                  (NamedEntity, OrderedNamedEntity)
import           WikiEL.WikiNamedEntityTagger          (PreNE(..),resolvedUID)
import qualified NLP.Type.NamedEntity       as N

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
                      deriving(Eq)
-- w : type of word token
type EMInfo w = (IRange, Vector w, PreNE)
type EntityMention w = UIDCite EntityMentionUID (EMInfo w)

entityName :: EMInfo Text -> Text
entityName (_, ws, _) = T.intercalate " " (toList ws)

entityUID :: EntityMention w -> Either String ItemID 
entityUID m = resolvedUID tag
  where
    (_,_,tag) = _info m

entityIRange :: EntityMention a -> IRange
entityIRange (Cite _ _ (range, _, _)) = range
entityIRange (Self _   (range, _, _)) = range

mentionedEntityName :: EntityMention Text -> Text
mentionedEntityName em = entityName (_info em)

toString :: EMInfo Text -> String
toString em@(range, ws, tag) = show range ++ " \"" ++ T.unpack (entityName em) ++  "\", " ++show tag

instance (Show a) => Show (UIDCite a (EMInfo Text))  where
  show (Cite uid ref info) = "Cite {" ++ show uid ++ " cites " ++ show ref ++ ",\t" ++ toString info ++ "}"
  show (Self uid info) = "Self {" ++ show uid  ++ ",\t" ++ toString info ++ "}"

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


