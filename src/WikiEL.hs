{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiEL 
  ( module WikiEL
  , module WikiEL.WordNet
  , EL.entityName
  , EL.entityUIDcandidates
  , EL.mentionedEntityName
  , EL.hasResolvedUID
  , EL.entityLinkings
  , EL.EntityMention
  , EMP.filterEMbyPOS
  , WEC.loadFiles
  , ED.loadWikipageMapping
  , ED.matchToSimilar
  , ED.tryDisambiguate
  , ItemID
  ) where

import           Data.Text                                    (Text)  
import qualified Data.Vector                        as V

import           NLP.Type.PennTreebankII                      (POSTag(..))
import           NLP.Type.NamedEntity                         (NamedEntityClass,NamedEntityFrag(..))

import           WikiEL.Type.Wikidata                         (ItemID)
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,namedEntityAnnotator)
import           WikiEL.WikiEntityTagger                      (NameUIDTable,loadWETagger)
import           WikiEL.WikiEntityClass                       (WikiuidNETag,ItemClass)
import           WikiEL.EntityLinking                         (EntityMention,entityLinkings,buildEntityMentions)
import qualified WikiEL.EntityLinking               as EL
import qualified WikiEL.EntityMentionPruning        as EMP
import qualified WikiEL.ETL.Util                    as U
import qualified WikiEL.WikiEntityClass             as WEC
import qualified WikiEL.EntityDisambiguation        as ED
import           WikiEL.Type.FileFormat
import           WikiEL.WordNet -- for WordNet synset lookup. 

extractEntityMentions :: NameUIDTable -> WikiuidNETag -> [(Text, NamedEntityClass)] -> [EntityMention Text]
extractEntityMentions wikiTable uidNEtags neTokens = linked_mentions
  where    
    stanford_nefs  = map (uncurry NamedEntityFrag) neTokens
    named_entities = getStanfordNEs stanford_nefs
    wiki_entities  = namedEntityAnnotator wikiTable stanford_nefs
    wiki_named_entities = resolveNEs uidNEtags named_entities wiki_entities

    words    = V.fromList (map fst neTokens)
    mentions = buildEntityMentions words wiki_named_entities
    linked_mentions = entityLinkings mentions

extractFilteredEntityMentions :: NameUIDTable -> WikiuidNETag -> [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text]
extractFilteredEntityMentions wikiTable uidNEtags tokens = filtered_mentions
  where    
    neTokens = map (\(x,y,z)->(x,y)) tokens
    poss     = map (\(x,y,z)->z)     tokens
    all_linked_mentions = extractEntityMentions wikiTable uidNEtags neTokens
    filtered_mentions   = EMP.filterEMbyPOS (V.fromList poss) all_linked_mentions
    
loadEMtagger :: EntityReprFile -> [(ItemClass, ItemIDFile)] -> IO( [(Text, NamedEntityClass)] -> [EntityMention Text] )
loadEMtagger wikiNameFile uidTagFiles = do
  wikiTable <- loadWETagger  wikiNameFile
  uidNEtags <- WEC.loadFiles uidTagFiles
  let
    emTagger = extractEntityMentions wikiTable uidNEtags
  return emTagger

loadFEMtagger :: EntityReprFile -> [(ItemClass, ItemIDFile)] -> IO( [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text] )
loadFEMtagger wikiNameFile uidTagFiles = do
  wikiTable <- loadWETagger  wikiNameFile
  uidNEtags <- WEC.loadFiles uidTagFiles
  let
    femTagger = extractFilteredEntityMentions wikiTable uidNEtags
  return femTagger

