{-# LANGUAGE BangPatterns #-}

module WikiEL.Graph where

import           Data.Int                              (Int32, Int64)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as GV
import           Control.Monad.ST                      (runST)

import           WikiEL.BinarySearch                   (binarySearchLR,binarySearchLRBy)

import qualified Data.List as L

isIn :: (UV.Unbox a, Ord a) => UV.Vector a -> a -> Bool
isIn vals = f sorted
  where
    sorted = UV.modify sort vals
    f vs x = runST $ do
      mvec <- UV.unsafeThaw vs
      (beg,end) <- binarySearchLR mvec x
      return ((end-beg)/=0)

-- NOTE: ordering is not preserved.
unique :: (UV.Unbox a, Ord a) => UV.Vector a -> UV.Vector a
unique vs = UV.foldl' f UV.empty sorted
  where
    sorted = UV.modify sort vs
    f accum v | UV.null accum = UV.singleton v
    f accum v | UV.last accum == v = accum
    f accum v = UV.snoc accum v

type Dist = Int32

{-
-- FlexibleInstances 
-- TypeSynonymInstances 

class Ord n => Edg e where
  f :: e -> n
  t :: e -> n
class Edg e where
  --f :: (Ord n) => e -> n
  --t :: (Ord n) => e -> n
  f :: e -> n
  t :: e -> n
  

instance Edg (Int32,Int32) where
  f  (x,_) = x
  t  (_,x) = x
-}
data Direction = From | To
               deriving (Show, Eq)

data SortedEdges e = Sorted Direction e

inverse :: Direction -> Direction
inverse From = To
inverse To = From

getNode :: Ord a => Direction -> (a,a) -> a 
getNode From (x,_) = x
getNode To   (_,x) = x
from,to :: Ord a => (a,a) -> a
from = getNode From
to   = getNode To

orderingByFrom,orderingByTo :: Ord a => (a,a) -> (a,a) -> Ordering
orderingByFrom left@(lf, _) right@(rf, _) = compare lf rf
orderingByTo   left@(_ ,lt) right@(_ ,rt) = compare lt rt

edgeOrdering :: Ord a => Direction -> (a,a) -> (a,a) -> Ordering
edgeOrdering From = orderingByFrom
edgeOrdering To   = orderingByTo

sortEdges :: (UV.Unbox a, Ord a) => Direction -> UV.Vector (a,a) -> (Direction, UV.Vector (a,a))
sortEdges From edges = (From, UV.modify (sortBy orderingByFrom) edges)
sortEdges To   edges = (To, UV.modify (sortBy orderingByTo)   edges)

{-
  returns edges inward/outward the input node, depends on `direction`(ordering) of the sorted edges
  direction : defines ordering between edges. If it orders by from side of edges,
         `neighbor` returns edges starting from the input node.
-}
neighbor :: (UV.Unbox a, Ord a) => (Direction, UV.Vector (a,a)) -> a -> UV.Vector a
neighbor (direction,sorted) node = runST $ do
  let
    comp = edgeOrdering direction
    destDirection = inverse direction
  mvec <- UV.unsafeThaw sorted
  (beg, end) <- binarySearchLRBy comp mvec (node,node)
  return (UV.map (getNode destDirection) (UV.slice beg (end-beg) sorted))

{-
fn :: a -> UV.Vector (a,a)
In this module, `fn` arguments denote partialy evaluated `neighbor` functions. 
It returns edges connedted to the input node of type `a`.
Direction of the edge is determined by the `comp` argument of the `neighbor`
-}

accumReachable :: (UV.Unbox a, Ord a) => UV.Vector (a,Dist) -> Dist -> (a -> UV.Vector a) -> (UV.Vector a, Dist) ->  UV.Vector (a,Dist)
accumReachable accum cutoff fn (frontiers,dist) | cutoff==dist = accum
accumReachable accum cutoff fn (frontiers,dist) = accumReachable (UV.concat [ns, accum]) cutoff fn (nexts, dist+1)
  where
    reachable = UV.concatMap fn frontiers -- nodes one step away to the frontier nodes
    news = UV.filter (not . isIn (UV.map fst accum)) reachable -- `reachable` edges that are not already visited
    nexts = unique news -- remove duplicated edges in `news`
    ns = UV.map (\x -> (x, dist+1)) nexts -- new frontier nodes

-- returns nodes of forward/backward distance up to `cutoff` starting from the input `node` in a directed graph `dEdges`
nodesForward,nodesBackward :: (UV.Unbox a, Ord a) => UV.Vector (a,a) -> a -> Dist -> UV.Vector (a,Dist)
nodesForward dEdges node cutoff  = accumReachable accum cutoff dForwardEdges (UV.fromList [node],0)
  where
    accum = UV.fromList [(node,0)]
    dForwardEdges  = neighbor (sortEdges From dEdges)
nodesBackward dEdges node cutoff = accumReachable accum cutoff dBackwardEdges (UV.fromList [node],0)
  where
    accum = UV.fromList [(node,0)]
    dBackwardEdges = neighbor (sortEdges To dEdges)

{-
-- Vector versions
accumPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> UV.Vector a -> V.Vector (UV.Vector a)
accumPaths fn path = UV.foldl' f V.empty (UV.map snd (fn from))
  where
    from = UV.last path
    f accum to = V.snoc accum (UV.snoc path to)

allPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector (a,a)) -> a -> Dist -> V.Vector (UV.Vector a)
allPaths fn node cutoff = f 0 (V.singleton (UV.singleton node))
  where
    f dist paths | dist==cutoff = paths
    f dist paths = f (dist+1) (V.concatMap (accumPaths fn) paths)
-}

accumPaths :: (UV.Unbox a, Ord a) => (a -> UV.Vector a) -> UV.Vector a -> [UV.Vector a]
accumPaths fn path = UV.foldl' f [] (fn (UV.last path))
  where
    f !accum !to = UV.snoc path to : accum

-- all paths of length `cutoff`, starting from the input `node`
allPathsOf :: (UV.Unbox a, Ord a) => (a -> UV.Vector a) -> a -> Dist -> [UV.Vector a]
allPathsOf fn node cutoff = f 0 [UV.singleton node]
  where
    f dist paths | dist==cutoff = paths
    f dist paths = f (dist+1) (L.concatMap (accumPaths fn) paths)

-- all paths of length UPTO `cutoff`, starting from the input `node`
allPathsUpto :: (UV.Unbox a, Ord a) => (a -> UV.Vector a) -> a -> Dist -> [UV.Vector a]
allPathsUpto fn node cutoff = f [] 0 [UV.singleton node]
  where
    f accum dist paths | dist==cutoff = accum ++ paths
    f accum dist paths = f (accum ++ paths) (dist+1) nexts
      where
        nexts = L.concatMap (accumPaths fn) paths


accumIf :: (t -> t -> Ordering) -> [(t, t)] -> [t] -> [t] -> [(t, t)]
accumIf comp accum [] _ = accum
accumIf comp accum _ [] = accum
accumIf comp accum lb@(l:ls) rb@(r:rs) | comp l r == EQ = accumIf comp ((l,r):accum) ls rs
accumIf comp accum lb@(l:ls) rb@(r:rs) | comp l r == LT = accumIf comp accum ls rb
accumIf comp accum lb@(l:ls) rb@(r:rs) | comp l r == GT = accumIf comp accum lb rs

neighborOverlap :: (UV.Unbox a, Ord a) => UV.Vector (a,Dist) -> UV.Vector (a,Dist) -> [((a,Dist),(a,Dist))]
neighborOverlap dists1 dists2 = accumIf compFst [] (UV.toList lhs) (UV.toList rhs)
  where
    compFst (ln,_) (rn,_) = compare ln rn
    lhs = UV.modify (sortBy compFst) dists1
    rhs = UV.modify (sortBy compFst) dists2

destOverlap :: (UV.Unbox a, Ord a) => [UV.Vector a] -> [UV.Vector a] -> [(UV.Vector a,UV.Vector a)]
destOverlap left right = accumIf compLast [] (V.toList ls) (V.toList rs)
  where
    compLast l r = compare (UV.last l) (UV.last r)
    ls = GV.modify (sortBy compLast) (V.fromList left)
    rs = GV.modify (sortBy compLast) (V.fromList right)
    --ls = L.sortBy compLast (B.toList left)
    --rs = L.sortBy compLast (B.toList right)

