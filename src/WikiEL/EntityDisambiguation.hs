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

loadAndSortEdges :: FilePath -> IO SortedGraph
loadAndSortEdges edgeFiles = do
    cc@(G.E.Graph edges names) <- G.E.applyLines G.E.loadGraph edgeFiles
    let
      sorted = G.sortEdges G.From edges
    return (SortedGraph sorted names)

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
    toTitle titles (NET.AmbiguousUID (ids,_)) = mapMaybe (`M.lookup` titles) ids
    toTitle titles (NET.Resolved (id,_))      = mapMaybe (`M.lookup` titles) [id]
    toTitle titles _                      = []

updateNE :: (NET.PreNE->NET.PreNE) -> EntityMention a -> EntityMention a
updateNE f (EL.Self id     info@(range,vec,ne)) = EL.Self id     (range,vec,f ne)
updateNE f (EL.Cite id ref info@(range,vec,ne)) = EL.Cite id ref (range,vec,f ne)

tryDisambiguate :: WEC.WikiuidNETag -> (M.Map ItemID Text, M.Map Text ItemID) -> ([Text] -> [Text] -> Maybe (a,Text,Text)) -> [EntityMention b] -> [EntityMention b]
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


disambiguateMentions (SortedGraph sorted names) uidTag titles mentions = filter EL.hasResolvedUID disambiguated
  where

    hash = H.wordHash
    paths len wp1 wp2 = G.destOverlapUpto (G.neighbor sorted) len (hash wp1) (hash wp2)
    f x y = length (paths 1 x y)    
    dms = tryDisambiguate uidTag titles (matchToSimilar f 3) mentions
    disambiguated = EL.entityLinkings dms

