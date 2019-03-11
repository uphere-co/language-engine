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




