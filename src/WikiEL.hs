{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiEL 
  ( module WikiEL
  , module WikiEL.WordNet
  , EL.entityName
  , EL.mentionedEntityName  
  , EMP.filterEMbyPOS
  ) where

import           Data.Text                                    (Text)  
import           Data.Vector                                  (fromList)
import           NLP.Type.PennTreebankII                      (POSTag(..))
import           NLP.Type.NamedEntity                         (NamedEntityClass,NamedEntityFrag(..))
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,namedEntityAnnotator)
import           WikiEL.WikiEntityTagger                      (NameUIDTable,loadWETagger)
import           WikiEL.WikiEntityClass                       (WikiUID2NETag,ItemClass,fromFiles)
import           WikiEL.EntityLinking                         (EntityMention,entityLinkings,buildEntityMentions)
import qualified WikiEL.EntityLinking               as EL
import qualified WikiEL.EntityMentionPruning        as EMP

import           WikiEL.Type.FileFormat
import           WikiEL.WordNet -- for WordNet synset lookup. 


extractEntityMentions :: NameUIDTable -> WikiUID2NETag -> [(Text, NamedEntityClass)] -> [EntityMention Text]
extractEntityMentions wikiTable uid2tag neTokens = linked_mentions
  where    
    stanford_nefs = map (uncurry NamedEntityFrag) neTokens
    named_entities =  getStanfordNEs stanford_nefs
    wiki_entities = namedEntityAnnotator wikiTable uid2tag stanford_nefs
    wiki_named_entities = resolveNEs named_entities wiki_entities

    words = fromList (map fst neTokens)
    mentions = buildEntityMentions words wiki_named_entities
    linked_mentions = entityLinkings mentions

extractFilteredEntityMentions :: NameUIDTable -> WikiUID2NETag -> [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text]
extractFilteredEntityMentions wikiTable uid2tag tokens = filtered_mentions
  where    
    neTokens = map (\(x,y,z)->(x,y)) tokens
    poss     = map (\(x,y,z)->z)     tokens
    all_linked_mentions = extractEntityMentions wikiTable uid2tag neTokens
    filtered_mentions = EMP.filterEMbyPOS (fromList poss) all_linked_mentions
    
loadEMtagger :: EntityReprFile -> [(ItemClass, ItemIDFile)] -> IO( [(Text, NamedEntityClass)] -> [EntityMention Text] )
loadEMtagger wikiNameFile uid2tagFiles = do
  wikiTable <- loadWETagger  wikiNameFile
  uid2tag <- fromFiles uid2tagFiles
  let
    emTagger = extractEntityMentions wikiTable uid2tag
  return emTagger

loadFEMtagger :: EntityReprFile -> [(ItemClass, ItemIDFile)] -> IO( [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text] )
loadFEMtagger wikiNameFile uid2tagFiles = do
  wikiTable <- loadWETagger  wikiNameFile
  uid2tag <- fromFiles uid2tagFiles
  let
    femTagger = extractFilteredEntityMentions wikiTable uid2tag
  return femTagger


