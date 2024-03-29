{-# LANGUAGE OverloadedStrings #-}

import           System.Environment                    (getArgs)
import           Data.Text                             (Text)
import           Data.Maybe                            (mapMaybe,catMaybes)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T.IO
import qualified Data.Char                     as C
import qualified Data.Text.Encoding            as T.E
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Vector.Unboxed           as UV

import qualified Graph                         as G
import qualified Graph.ETL                     as G.E
import qualified Graph.Internal.Hash           as H
import           WikiEL.Type                           (NodeNames)
import qualified WikiEL.ETL.Util               as E.U

stripEdges :: [a] -> Maybe [a]
stripEdges vs | length vs <2 = Nothing
stripEdges vs = Just ((tail . init) vs)

-- type ToBString   = M.Map H.WordHash G.E.BString
type SortedEdges = (G.Direction, UV.Vector (H.WordHash, H.WordHash))
type NodeCounts  = M.Map Text Int
type Path        = [Text]


{-|
  It accumulates counts of nodes in a path, except both edges.
-}
consume :: NodeNames -> SortedEdges -> NodeCounts -> Path -> NodeCounts
consume _ _ cs ns | length ns < 2 = cs
consume names sorted cs (a:b:ns) = consume names sorted cs' ns
  where
    hashT = H.wordHash
    showPath invs path = map T.E.decodeUtf8 (catMaybes (UV.foldl' f [] path))
      where
        f accum hash = M.lookup hash invs : accum
    showPathPair names' (x,y) = reverse (showPath names' y) ++ tail (showPath names' x)

    getPaths len wp1 wp2 = G.destOverlapUpto (G.neighbor sorted) len (hashT wp1) (hashT wp2)
    paths = map (showPathPair names) (getPaths 1 a b)
    fs = L.foldl' f
      where f accum v = M.insertWith (+) v 1 accum
    cs' = L.foldl' fs cs (mapMaybe stripEdges paths)
consume _ _ _ _ = error "consume"

{-|
  1. Randomly selects two nodes.
  2. Get paths between the two nodes.
  3. Count nodes in the paths and accumulate them.
  4. Nodes with top N counts are the 'noisy hub nodes' and printed.
-}
countNodes :: FilePath -> FilePath -> Int -> IO [(T.Text, Int)]
countNodes linkfile nodeSampleFile cutoff = do
  G.E.Graph edges names <- G.E.applyLines G.E.loadGraph linkfile
  let
    sortedEdges = G.sortEdges G.From  edges
  ls <- E.U.readlines nodeSampleFile
  let
    cs = consume names sortedEdges M.empty (take 1000000 ls)
    vs = L.sortOn ((\x -> -x) . snd) (M.toList cs)
    filters = take cutoff vs
  -- mapM_ print filters
  mapM_ T.IO.putStrLn $ filter (not . C.isLower . T.head ) (map fst filters)
  return filters

main :: IO ()
main = do
  args <- getArgs
  let
    -- Regarding how to produce linkefile and weightfile, see README.md
    [linkfile,weightfile] = args
    n_node = 100  -- cutoff for a number of noisy hub nodes to select
  _filters <- countNodes linkfile weightfile n_node
  putStrLn ""
