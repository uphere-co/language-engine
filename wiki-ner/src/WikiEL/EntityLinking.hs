{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module WikiEL.EntityLinking where

import           Data.List                             (foldl')
import           Data.Text                             (Text)
import qualified Data.Text                  as T
import           Data.Vector                           (Vector,toList)
--
import           WikiEL.Misc                           (relativePos,strictSlice,subVector) 
import           WikiEL.Type                           (EMInfo,EntityMention,EntityMentionUID(..),IRange(..)
                                                       ,PreNE(..),RelativePosition(..)
                                                       ,UIDCite(..))
import           WikiEL.Type.Wikidata                  (ItemID)
import           NLP.Type.NamedEntity                  (NamedEntity, OrderedNamedEntity)
import           WikiEL.WikiNamedEntityTagger          (isResolved,resolvedUID,mayCite,uidCandidates)
import qualified NLP.Type.NamedEntity       as N

mayRefer :: NamedEntity -> NamedEntity -> Bool
mayRefer src target = (N._type src == N._type target) && T.isInfixOf (N._str src) (N._str target)

canRefer :: OrderedNamedEntity -> OrderedNamedEntity -> Bool
canRefer src target = (N._order src > N._order target) && mayRefer (N._entity src) (N._entity target)

entityName :: EMInfo Text -> Text
entityName (_, ws, _) = T.intercalate " " (toList ws)

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

entityIRange :: EntityMention a -> IRange
entityIRange mention = range
  where (range, _, _) = _info mention

entityPreNE :: EntityMention a -> PreNE
entityPreNE mention = ne
  where (_,_, ne) = _info mention

hasResolvedUID :: EntityMention a -> Bool
hasResolvedUID mention  = isResolved (entityPreNE mention)

buildEntityMentions :: Vector w -> [(IRange, PreNE)] -> [EntityMention w]
buildEntityMentions text wikiNEs = zipWith Self uids mentions
  where
    uids     = map EntityMentionUID [1..]
    mentions = map (\(range,e) -> (range, subVector range text, e)) wikiNEs


tryEntityLink :: Eq a => EMInfo a -> EMInfo a -> Maybe (EMInfo a)
tryEntityLink (trange, twords, tNE) (srange, swords, sNE) =
  f (relativePos trange srange) (strictSlice swords twords) tNE sNE
  where
    g ttag (Resolved (_, s))       = s==ttag
    g ttag (UnresolvedUID stag)    = mayCite stag ttag
    g ttag (AmbiguousUID (_,stag)) = mayCite stag ttag
    g _ttag _                      = False
    
    f pos textMatch (Resolved (uid,ttag)) src | pos==LbeforeR && textMatch && g ttag src =
      Just (srange,swords, Resolved (uid,ttag))
    f _ _ _ _ = Nothing

entityLinking :: Eq a => [EntityMention a] -> EntityMention a -> EntityMention a
entityLinking targets source = foldr f source targets
  where
    f target src@(Self idx info)       =
      case tryEntityLink (_info target) info of
        Nothing   -> src
        Just linked -> Cite idx (_uid target) linked
    f _      src = src

{-
  entityLinkings tries to resolve unresolved or ambiguous entity mentions using :
    1. phrase inclusion
    2. entity type matching (see WikiEL.WikiEntityClass.mayCite for details)
-}
entityLinkings :: Eq a => [EntityMention a] -> [EntityMention a]
entityLinkings srcs = reverse (foldl' f [] srcs)
  where
    f targets src = entityLinking targets src : targets

