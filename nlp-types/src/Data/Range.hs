{-# LANGUAGE BangPatterns #-}

--
-- This is a module that collects operation and comparison on range
--
module Data.Range where

import           Data.Either   (partitionEithers)
import           Data.Foldable (any)
import           Data.List     (inits,mapAccumL)
--
import           Data.Bitree
--
import           Prelude    hiding (max) 


type Range = (Int,Int)

isInside :: Int -> Range -> Bool
x `isInside` (x1,y1) = x1 <= x && x <= y1

isInsideR :: Range -> Range -> Bool
(x0,y0) `isInsideR` (x1,y1) = x1 <= x0 && y0 <= y1

isStrictlyInsideR :: Range -> Range -> Bool
(x0,y0) `isStrictlyInsideR` (x1,y1) = (x1 < x0 && y0 <= y1) || (x1 <= x0 && y0 < y1)

elemIsInsideR :: Foldable t => Range -> t Range -> Bool
elemIsInsideR x ys = any (\y -> x `isInsideR` y) ys

elemIsStrictlyInsideR :: Foldable t => Range -> t Range -> Bool
elemIsStrictlyInsideR x ys = any (\y -> x `isStrictlyInsideR` y) ys

elemRevIsInsideR :: Foldable t => Range -> t Range -> Bool
elemRevIsInsideR x ys = any (\y -> y `isInsideR` x) ys

elemRevIsStrictlyInsideR :: Foldable t => Range -> t Range -> Bool
elemRevIsStrictlyInsideR x ys = any (\y -> y `isStrictlyInsideR` x) ys

-- | Bubble sort of ranges. Since a list of range is a partially ordered set,
--   we mark non-included ranges as Left.
--
rootRange :: [Range] -> (Range, [Either Range Range])
rootRange []     = error "rootRange"
rootRange rs@(r:_) = 
                     let res = mapAccumL (\(!rmax) rlst -> go rmax rlst) r (tail (inits rs ++ [rs]))
                     in (fst res, last (snd res))
  where
    go x xs = mapAccumL f x xs
    
    f !max x | x `isInsideR` max = (max, Right x)
             | max `isInsideR` x = (x  , Right x)
             | otherwise         = (max, Left  x)



-- | Recursively apply range bubble sort to get all partial ordering relations
--   that exists in a range list.
--   
partitionRanges :: [Range] -> [(Range,[Range])]
partitionRanges rngs = let (rmax,rngs') = rootRange rngs
                           (outside,inside') = partitionEithers rngs'
                           inside = filter (not . (== rmax)) inside'
                       in case outside of
                            [] -> [(rmax,inside)]
                            _  -> (rmax,inside) : partitionRanges outside


-- | Tree structure out of a list of ranges by inclusion relation.
--
rangeTree :: [Range] -> [Bitree Range Range]
rangeTree []   = []
rangeTree rngs = let ps = partitionRanges rngs
                     f (rmax,[]) = PL rmax
                     f (rmax,rs) = PN rmax (rangeTree rs)
                 in map f ps
