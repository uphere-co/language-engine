{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module WikiEL 
  ( module WikiEL
  , module WikiEL.WordNet
  , EL.entityName
  , EL.mentionedEntityName
  , EL.hasResolvedUID
  , EL.EntityMention
  , EMP.filterEMbyPOS
  , ItemID
  ) where

import           Data.Text                                    (Text)  
import qualified Data.Vector                        as V

import           NLP.Type.PennTreebankII                      (POSTag(..))
import           NLP.Type.NamedEntity                         (NamedEntityClass,NamedEntityFrag(..))

import           WikiEL.WikiNamedEntityTagger                 (resolveNEs,getStanfordNEs,namedEntityAnnotator)
import           WikiEL.WikiEntityTagger                      (NameUIDTable,loadWETagger)
import           WikiEL.WikiEntityClass                       (WikiuidNETag,ItemClass,loadFiles,fromNEClass)
import           WikiEL.EntityLinking                         (EntityMention,entityLinkings,buildEntityMentions)
import qualified WikiEL.EntityLinking               as EL
import qualified WikiEL.EntityMentionPruning        as EMP
import qualified WikiEL.ETL.Util                    as U
import           WikiEL.Type.FileFormat
import           WikiEL.WordNet -- for WordNet synset lookup. 

-- To be saparated out with the disambiguation module
import           Data.Maybe                                   (fromJust,mapMaybe)
import           WikiEL.Type.Wikidata                         (ItemID)
import           WikiEL.WikiNamedEntityTagger                 (PreNE(..))
import qualified Data.Map                           as M
import qualified WikiEL.ETL.Parser                  as P
import qualified NLP.Type.NamedEntity               as NE

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
  uidNEtags <- loadFiles uidTagFiles
  let
    emTagger = extractEntityMentions wikiTable uidNEtags
  return emTagger

loadFEMtagger :: EntityReprFile -> [(ItemClass, ItemIDFile)] -> IO( [(Text, NamedEntityClass, POSTag)] -> [EntityMention Text] )
loadFEMtagger wikiNameFile uidTagFiles = do
  wikiTable <- loadWETagger  wikiNameFile
  uidNEtags <- loadFiles uidTagFiles
  let
    femTagger = extractFilteredEntityMentions wikiTable uidNEtags
  return femTagger

loadWikipageMapping :: P.WikiTitleMappingFile -> IO (M.Map ItemID Text, M.Map Text ItemID)
loadWikipageMapping filename = do
  lines <- U.readlines (P.unWikiTitleMappingFile filename)
  let
    id2title = map P.wikititleMapping lines
    title2id = map (\(x,y)->(y,x)) id2title
  return (M.fromList id2title, M.fromList title2id)

toWikipages :: M.Map ItemID Text -> EntityMention a -> [Text]
toWikipages titles mention = toTitle titles (EL.entityPreNE mention)
  where
    toTitle titles (AmbiguousUID (ids,_)) = mapMaybe (`M.lookup` titles) ids
    toTitle titles (Resolved (id,_))      = mapMaybe (`M.lookup` titles) [id]
    toTitle titles _                      = []

updateNE :: (PreNE->PreNE) -> EntityMention a -> EntityMention a
updateNE f (EL.Self id     info@(range,vec,ne)) = EL.Self id     (range,vec,f ne)
updateNE f (EL.Cite id ref info@(range,vec,ne)) = EL.Cite id ref (range,vec,f ne)

tryDisambiguate :: (M.Map ItemID Text, M.Map Text ItemID) -> ([Text] -> [Text] -> Maybe (a,Text,Text)) -> [EntityMention b] -> [EntityMention b]
tryDisambiguate (i2t,t2i) fTD mentions = map (updateNE f) mentions
  where
    refs = concatMap (toWikipages i2t) (filter EL.hasResolvedUID mentions)
    f x@(AmbiguousUID ([],_))= x
    f x@(AmbiguousUID (ids,stag)) = g (fTD refs titles)
      where
        titles = mapMaybe (`M.lookup` i2t) ids
        g (Just (score,ref,title)) = Resolved (fromJust $ M.lookup title t2i, fromNEClass stag)
        g Nothing                  = x
    f x                  = x


-- n : Node, s : Score type
mostSimilar :: (Num s, Ord s, Ord n) => (n->n->s) -> s-> n -> [n] -> Maybe (s,n,n)
mostSimilar f cutoff ref ns = fCutoff maxsim
  where
    maxsim = maximum $ map (\n -> (f ref n, ref, n)) ns
    fCutoff sim@(score,_,_) | score>cutoff = Just sim
    fCutoff _ = Nothing

matchToSimilar :: (Num s, Ord s, Ord n) => (n->n->s) -> s -> [n] -> [n] -> Maybe (s,n,n)
matchToSimilar f cutoff refs ns = mayMax ss
  where
    ss = mapMaybe (\ref -> mostSimilar f cutoff ref ns) refs
    mayMax [] = Nothing
    mayMax vs = Just (maximum vs)
    
