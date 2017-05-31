{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiEL where

import           Data.Text                                    (Text)  
import           Data.Vector                                  (fromList)
import           NLP.Type.NamedEntity                         (NamedEntityClass,NamedEntityFrag(..))
import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,namedEntityAnnotator)
import           WikiEL.WikiEntityTagger                      (NameUIDTable,loadWETagger)
import           WikiEL.WikiEntityClass                       (WikiUID2NETag,fromFiles)
import           WikiEL.EntityLinking                         (EntityMention,entityLinkings,buildEntityMentions)

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

loadEMtagger :: FilePath -> [(NamedEntityClass, FilePath)] -> IO( [(Text, NamedEntityClass)] -> [EntityMention Text] )
loadEMtagger wikiNameFile uid2tagFiles = do
  wikiTable <- loadWETagger  wikiNameFile
  uid2tag <- fromFiles uid2tagFiles
  let
    emTagger = extractEntityMentions wikiTable uid2tag
  return emTagger
