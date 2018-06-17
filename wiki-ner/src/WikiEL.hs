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
  , EMP.filterEM
  , WET.loadWETagger
  , WEC.loadFiles
  , ED.loadWikipageMapping
  , ED.loadAndSortEdges
  , ED.matchToSimilar
  , ED.tryDisambiguate
  , LD.loadCompanySymbol
  , ItemID
  ) where

import           Data.Text                                  (Text)  
import qualified Data.Vector                        as V
--
import qualified Graph                              as G
import qualified Graph.Internal.Hash                as H
import           NLP.Type.PennTreebankII                    (POSTag(..))
import           NLP.Type.NamedEntity                       (NamedEntityClass,NamedEntityFrag(..))
--
import qualified WikiEL.EntityDisambiguation        as ED
import           WikiEL.EntityLinking                       (entityLinkings,buildEntityMentions)
import qualified WikiEL.EntityLinking               as EL
import qualified WikiEL.EntityMentionPruning        as EMP
import qualified WikiEL.ETL.LoadData                as LD
import           WikiEL.Type                                (EntityMention,ItemClass,NameUIDTable,SortedGraph(..),WikiuidNETag)
import           WikiEL.Type.FileFormat

import           WikiEL.Type.Wikidata                       (ItemID)
import qualified WikiEL.WikiEntityClass             as WEC
import qualified WikiEL.WikiEntityTagger            as WET
import           WikiEL.WikiNamedEntityTagger               (resolveNEs,getStanfordNEs,namedEntityAnnotator)
import           WikiEL.WordNet -- for WordNet synset lookup. 


{- |
This is a module for Wikipedia entity linking. 
Two high-level functions for client uses.

## `loadFEMtagger` does following :
- Text-matching based entity linking of phrases to Wikidata UID. Has many ambiguities
- Drop possibilities that incosistent with CoreNLP named entity recognizer
- Drop entity mentions that are not noun phrases.
## `disambiguateMentions` does following :
- For a given entity mention with multiple UID candidates, pick one which is
  - high relatedness score to resolved entity mentions
  - a relatedness score of a pair of entities is based on paths of Wikipedia interlinks
  - See `WikiEL.EntityDisambiguation` for details.
-}


extractEntityMentions :: NameUIDTable -> WikiuidNETag -> [(Text, NamedEntityClass)] -> [EntityMention Text]
extractEntityMentions wikiTable uidNEtags neTokens = linked_mentions
  where    
    stanford_nefs  = map (uncurry NamedEntityFrag) neTokens
    named_entities = getStanfordNEs stanford_nefs
    wiki_entities  = namedEntityAnnotator wikiTable stanford_nefs
    wiki_named_entities = resolveNEs uidNEtags named_entities wiki_entities

    ws    = V.fromList (map fst neTokens)
    mentions = buildEntityMentions ws wiki_named_entities
    linked_mentions = entityLinkings mentions

extractFilteredEntityMentions :: NameUIDTable -> WikiuidNETag -> [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text]
extractFilteredEntityMentions wikiTable uidNEtags tokens = filtered_mentions
  where    
    neTokens = map (\(x,y,_)->(x,y)) tokens
    poss     = map (\(_,_,z)->z)     tokens
    all_linked_mentions = extractEntityMentions wikiTable uidNEtags neTokens
    filtered_mentions   = EMP.filterEMbyPOS (V.fromList poss) all_linked_mentions

-- |loadEMtagger : a high level function for entity linking module
loadEMtagger :: EntityReprFile -> [(ItemClass, ItemIDFile)] -> IO( [(Text, NamedEntityClass)] -> [EntityMention Text] )
loadEMtagger wikiNameFile uidTagFiles = do
  wikiTable <- WET.loadWETagger  wikiNameFile
  uidNEtags <- WEC.loadFiles uidTagFiles
  let
    emTagger = extractEntityMentions wikiTable uidNEtags
  return emTagger

-- |loadFEMtagger : loadEMtagger + POS-based filtering for entity mentions
loadFEMtagger :: EntityReprFile -> [(ItemClass, ItemIDFile)] -> IO( [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text] )
loadFEMtagger wikiNameFile uidTagFiles = do
  wikiTable <- WET.loadWETagger  wikiNameFile
  uidNEtags <- WEC.loadFiles uidTagFiles
  let
    femTagger = extractFilteredEntityMentions wikiTable uidNEtags
  return femTagger


-- |disambiguateMentions : a high level function for client use to perform the entity mention disambiguation.
disambiguateMentions :: Eq a => SortedGraph -> WikiuidNETag -> ED.NameMappings -> [EntityMention a] -> [EntityMention a]
disambiguateMentions (SortedGraph sorted _names) uidTag titles mentions = filter EL.hasResolvedUID outputs
  where
    distance_cut = 1
    score_cut    = 3
    hash = H.wordHash
    paths len wp1 wp2 = G.destOverlapUpto (G.neighbor sorted) len (hash wp1) (hash wp2)
    f x y = length (paths distance_cut x y)    
    dms = ED.tryDisambiguate uidTag titles (ED.matchToSimilar f score_cut) mentions
    disambiguated = EL.entityLinkings dms
    outputs = EL.entityLinkings disambiguated
