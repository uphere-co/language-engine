module WikiEL.EntityDisambiguation
( module WikiEL.EntityDisambiguation
) where


import           Data.Maybe                            (fromJust,mapMaybe,catMaybes)
import           Data.Text                             (Text)
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import qualified Data.Vector.Unboxed           as UV

import           WikiEL.Type.Wikidata                         (ItemID)
import           WikiEL.EntityLinking                         (EntityMention)
import qualified NLP.Type.NamedEntity          as NE
import qualified Graph.ETL                     as G.E
import qualified Graph.Internal.Hash           as H  
import qualified WikiEL.Graph                  as G
import qualified WikiEL.EntityLinking          as EL
import qualified WikiEL.WikiNamedEntityTagger  as NET
import qualified WikiEL.WikiEntityClass        as WEC
import qualified WikiEL.ETL.Parser             as P
import qualified WikiEL.ETL.Util               as U


type SortedEdges = (G.Direction, UV.Vector (H.WordHash, H.WordHash))
type NodeNames   = M.Map H.WordHash G.E.BString
data SortedGraph = SortedGraph SortedEdges NodeNames


{-
loadAndSortEdges 
Input format : a text file of a list of directed edges. Node names are Wikipedia title
Input arguments :
- edgeFiles : a fullpath of an input text file.
Output arguments :
- sorted : a unboxed vector of (int,int), sorted by a first and then a second element.
- names : a hash map from int(hash values of nodes) to text(Wikipedia title)
-}
loadAndSortEdges :: FilePath -> IO SortedGraph
loadAndSortEdges edgeFiles = do
    cc@(G.E.Graph edges names) <- G.E.applyLines G.E.loadGraph edgeFiles
    let
      sorted = G.sortEdges G.From edges
    return (SortedGraph sorted names)

{-
loadWikipageMapping
Input format : See WikiEL.ETL.Parser.WikiTitleMappingFile
Output : a mapping between Wikidata UIDs and Wikipedia titles
Output arguments :
- id2title : UID to title
- title2uid : title to UID
-}
loadWikipageMapping :: P.WikiTitleMappingFile -> IO (M.Map ItemID Text, M.Map Text ItemID)
loadWikipageMapping filename = do
  lines <- U.readlines (P.unWikiTitleMappingFile filename)
  let
    id2title = map P.wikititleMapping lines
    title2id = map (\(x,y)->(y,x)) id2title
  return (M.fromList id2title, M.fromList title2id)

{-
Entity linking aims to map each entity mention to Wikipedia UID. 
Until an entity mention is fully resolved, it has UID candidates(one if resolved, many if ambiguous or unresolved).
toWikipages returns Wikipedia titles of UID candidates.
-}
toWikipages :: M.Map ItemID Text -> EntityMention a -> [Text]
toWikipages titles mention = toTitle titles (EL.entityPreNE mention)
  where
    toTitle titles ne = mapMaybe (`M.lookup` titles) (NET.uidCandidates ne)

updateNE :: (NET.PreNE->NET.PreNE) -> EntityMention a -> EntityMention a
updateNE f (EL.Self id     info@(range,vec,ne)) = EL.Self id     (range,vec,f ne)
updateNE f (EL.Cite id ref info@(range,vec,ne)) = EL.Cite id ref (range,vec,f ne)

{-
tryDisambiguate : a main funtion that does the named entity disambiguation.
1. Each entity mention is mapped to `[Text]`, a list of Wikipedia titles (of UID candidates)
2. fTD is a scoring function 
 - input : a pair of `[Text]` 
 - output : a similarity score and a most similar pair of Text, one from each input list.
3. Wikidata UID has types such as "Person", "Location", "Brand", and so on; listed in `WikiEL.WikiEntityClass`.
4. fTD only considers similarity. Types of Wikipedia UID(i.e. Title) is also considered for disambiguation.

Input arguments:
- uidNEtags : contains types of Wikidata UIDs
- (i2t,t2i) : Wikipeida title <-> Wikidata UID mapping
- fTD : the scoring function
- mentions : input entity mentions
Output : disambiguated entity mentions.
-}

type NameMappings = (M.Map ItemID Text, M.Map Text ItemID)
tryDisambiguate :: WEC.WikiuidNETag -> NameMappings -> ([Text] -> [Text] -> Maybe (a,Text,Text)) -> [EntityMention b] -> [EntityMention b]
tryDisambiguate uidNEtags (i2t,t2i) fTD mentions = map (updateNE f) mentions
  where
    refs = concatMap (toWikipages i2t) (filter EL.hasResolvedUID mentions)
    f x@(NET.AmbiguousUID ([],_))= x
    f x@(NET.AmbiguousUID (ids,stag)) = g (fTD refs titles)
      where
        titles = mapMaybe (`M.lookup` i2t) ids
        g (Just (score,ref,title)) | WEC.mayCite stag tag = NET.Resolved (uid,tag)
          where
            uid  = fromJust $ M.lookup title t2i
            tag = WEC.guessItemClass2 uidNEtags stag uid        
        g _                  = x
          
    f x                  = x


-- n : Node, s : Score type
mostSimilar :: (Num s, Ord s, Ord n) => (n->n->s) -> s-> n -> [n] -> Maybe (s,n,n)
mostSimilar f cutoff ref ns = fCutoff (mayMax ss)
  where
    ss = map (\n -> (f ref n, ref, n)) ns
    mayMax [] = Nothing
    mayMax vs = Just (maximum vs)
    fCutoff (Just sim@(score,_,_)) | score>cutoff = Just sim
    fCutoff _ = Nothing

matchToSimilar :: (Num s, Ord s, Ord n) => (n->n->s) -> s -> [n] -> [n] -> Maybe (s,n,n)
matchToSimilar f cutoff refs ns = mayMax ss
  where
    ss = mapMaybe (\ref -> mostSimilar f cutoff ref ns) refs
    mayMax [] = Nothing
    mayMax vs = Just (maximum vs)
