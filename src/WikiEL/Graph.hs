{-# LANGUAGE BangPatterns #-}

module WikiEL.Graph where

import           Data.Int                              (Int32, Int64)
import           Data.Vector.Algorithms.Intro          (sort, sortBy)
import           Control.Monad.ST                      (runST)
import qualified Data.Vector.Unboxed           as UV
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as GV
import qualified Data.List                     as L

import           WikiEL.BinarySearch                   (binarySearchLR,binarySearchLRBy)


isIn :: (UV.Unbox a, Ord a) => UV.Vector a -> a -> Bool
isIn vals = f
  where
    sorted = UV.modify sort vals
    f x = runST $ do
      mvec <- UV.unsafeThaw sorted
      (beg,end) <- binarySearchLR mvec x
      return ((end-beg)/=0)

-- NOTE: for all of unique, intersection*, union* functions, 
--       ordering is NOT preserved.
unique :: (UV.Unbox a, Ord a) => UV.Vector a -> UV.Vector a
unique vs = UV.foldl' f UV.empty sorted
  where
    sorted = UV.modify sort vs
    f accum v | UV.null accum = UV.singleton v
    f accum v | UV.last accum == v = accum
    f accum v = UV.snoc accum v

intersectionVec, unionVec :: (UV.Unbox a, Ord a) => UV.Vector a -> UV.Vector a -> UV.Vector a
intersectionVec lv rv = fst (UV.unstablePartition (isIn lv) rv)
unionVec        lv rv = lv UV.++ snd (UV.unstablePartition (isIn lv) rv)

intersectionVecBy :: (UV.Unbox a, Ord a) => (b->a) -> [b] -> [b] -> [b]
intersectionVecBy hash lv rv = fst (L.partition f rv)
  where
    lvec = UV.fromList (map hash lv)
    f x = isIn lvec (hash x)

unionVecBy :: (UV.Unbox a, Ord a) => (b->a) -> [b] -> [b] -> [b]
unionVecBy hash lv rv = lv ++ snd (L.partition f rv)
  where
    lvec = UV.fromList (map hash lv)
    f x = isIn lvec (hash x)


intersection :: (UV.Unbox a, Ord a) => [UV.Vector a] -> UV.Vector a
intersection [] = UV.empty
intersection vs@(v:vt) = L.foldl' intersectionVec v vt

intersectionBy :: (UV.Unbox a, Ord a) => (b->a) -> [[b]] -> [b]
intersectionBy hash [] = []
intersectionBy hash vs@(v:vt) = L.foldl' (intersectionVecBy hash) v vt


union :: (UV.Unbox a, Ord a) => [UV.Vector a] -> UV.Vector a
union [] = UV.empty
union vs@(v:vt) = L.foldl' unionVec v vt

unionBy :: (UV.Unbox a, Ord a) => (b->a) -> [[b]] -> [b]
unionBy hash [] = []
unionBy hash vs@(v:vt) = L.foldl' (unionVecBy hash) v vt


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

partition2 :: (a->a->Ordering) -> [a] -> [a] -> ([a],[a],[a],[a])
partition2 comp ls rs = f [] [] [] [] (V.toList sortedL) (V.toList sortedR)
  where
    sortedL = GV.modify (sortBy comp) (V.fromList ls)
    sortedR = GV.modify (sortBy comp) (V.fromList rs)
    -- ul : unmatched left
    -- ml : matched left
    -- ur : unmatched right
    -- mr : matched right
    f ul ml ur mr ls@(l:lt) rs@(r:rt) | comp l r == EQ = f ul (l:ml) ur (r:mr) lt rt
    f ul ml ur mr ls@(l:lt) rs@(r:rt) | comp l r == LT = f (l:ul) ml ur mr lt rs
    f ul ml ur mr ls@(l:lt) rs@(r:rt) | comp l r == GT = f ul ml (r:ur) mr ls rt
    f ul ml ur mr [] rs = (ul,ml,rs++ur,mr)
    f ul ml ur mr ls []  = (ls++ul,ml,ur,mr)

neighborOverlap :: (UV.Unbox a, Ord a) => UV.Vector (a,Dist) -> UV.Vector (a,Dist) -> [((a,Dist),(a,Dist))]
neighborOverlap dists1 dists2 = zip ml mr
  where
    compFst (ln,_) (rn,_) = compare ln rn
    lhs = UV.modify (sortBy compFst) dists1
    rhs = UV.modify (sortBy compFst) dists2
    (ul,ml, ur,mr) = partition2 compFst (UV.toList lhs) (UV.toList rhs)

destOverlap :: (UV.Unbox a, Ord a) => [UV.Vector a] -> [UV.Vector a] -> [(UV.Vector a,UV.Vector a)]
destOverlap left right = zip ml mr
  where
    compLast l r = compare (UV.last l) (UV.last r)
    ls = GV.modify (sortBy compLast) (V.fromList left)
    rs = GV.modify (sortBy compLast) (V.fromList right)
    (ul,ml, ur,mr) = partition2 compLast (V.toList ls) (V.toList rs)
    --ls = L.sortBy compLast (B.toList left)
    --rs = L.sortBy compLast (B.toList right)


destOverlapUpto :: (UV.Unbox a, Ord a) => (a -> UV.Vector a) -> Dist -> a -> a -> [(UV.Vector a,UV.Vector a)]
destOverlapUpto fn halfDist nodeL nodeR = f [] [] [UV.singleton nodeL] [UV.singleton nodeR] [] 0
  where
    -- ls, rs : remaining (unmatched) paths
    -- lfs, rfs : frontier paths
    f ls rs lfs rfs accum d | d == halfDist = accum
    f ls rs lfs rfs accum d = f ls' rs' lfs' rfs' (matched ++ accum) (d+1)
      where
        hash = UV.last
        comp l r = compare (hash l) (hash r)
        newPathsL = L.concatMap (accumPaths fn) lfs
        newPathsR = L.concatMap (accumPaths fn) rfs
        (ul1,ml1, ur1,mr1) = partition2 comp newPathsL newPathsR
        (ul2,ml2, ur2,mr2) = partition2 comp (ls++lfs) newPathsR
        (ul3,ml3, ur3,mr3) = partition2 comp newPathsL (rs++rfs)
        matched = zip (L.concat [ml1,ml2,ml3]) (L.concat [mr1,mr2,mr3])
        -- Note that
        -- newPathsR == unionVecBy hash mr1 mr2 ++ intersectionVecBy hash ur1 ur2
        -- newPathsL == unionVecBy hash ml1 ml3 ++ intersectionVecBy hash ul1 ul3
        -- ls++lfs   == ul2 + ml2
        -- rs++rfs   == ur3 + mr3
        ls' = ls++lfs ++ unionVecBy hash ul1 ul3
        rs' = rs++rfs ++ unionVecBy hash ur1 ur2
        lfs' = intersectionVecBy hash ul1 ul3
        rfs' = intersectionVecBy hash ur1 ur2

