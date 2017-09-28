{-# LANGUAGE OverloadedStrings #-}


import           Data.Text                             (Text)
import           Data.Maybe                            (mapMaybe,catMaybes)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Char                     as C
import qualified Data.Text.Encoding            as T.E
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Vector.Unboxed           as UV

import qualified WikiEL.Graph                  as G
import qualified Graph.ETL                     as G.E
import qualified Graph.Internal.Hash           as H
import qualified WikiEL.EntityDisambiguation   as ED
import qualified WikiEL.ETL.Util               as E.U

stripEdges :: [a] -> Maybe [a]
stripEdges vs | length vs <2 = Nothing
stripEdges vs = Just ((tail . init) vs)

type ToBString   = M.Map H.WordHash G.E.BString
type SortedEdges = (G.Direction, UV.Vector (H.WordHash, H.WordHash))
type NodeCounts  = M.Map Text Int
type Path        = [Text]


{-|

-}
consume :: ED.NodeNames -> SortedEdges -> NodeCounts -> Path -> NodeCounts
consume _ _ cs ns | length ns < 2 = cs
consume names sorted cs (a:b:ns) = consume names sorted cs' ns
  where
    --hash word = H.wordHash (T.pack word)
    hashT = H.wordHash
    --showPath invs path = catMaybes (UV.foldl' f [] path) where f accum hash = M.lookup hash invs : accum
    --showPathPairs names = mapM_  (print . (\(x,y)-> reverse (showPath names y) ++ tail (showPath names x)))
    showPath invs path = map T.E.decodeUtf8 (catMaybes (UV.foldl' f [] path))
      where
        f accum hash = M.lookup hash invs : accum
    showPathPair names (x,y) = reverse (showPath names y) ++ tail (showPath names x)
  
    getPaths len wp1 wp2 = G.destOverlapUpto (G.neighbor sorted) len (hashT wp1) (hashT wp2)
    paths = map (showPathPair names) (getPaths 1 a b)
    fs = L.foldl' f
      where f accum v = M.insertWith (+) v 1 accum
    cs' = L.foldl' fs cs (mapMaybe stripEdges paths)

countNodes :: FilePath -> FilePath -> Int -> IO [(T.Text, Int)]
countNodes linkfile nodeSampleFile cutoff = do
  cc@(G.E.Graph edges names) <- G.E.applyLines G.E.loadGraph linkfile
  let
    sortedEdges = G.sortEdges G.From  edges
  lines <- E.U.readlines nodeSampleFile
  let 
    cs = consume names sortedEdges M.empty (take 1000000 lines)    
    vs = L.sortOn ((\x -> -x) . snd) (M.toList cs)
    filters = take cutoff vs
  -- mapM_ print filters
  mapM_ T.IO.putStrLn $ filter (not . C.isLower . T.head ) (map fst filters)
  return filters

main :: IO ()
main = do
  filters <- countNodes "interlinks" "nodes.weighted.ran" 100
  print ""
