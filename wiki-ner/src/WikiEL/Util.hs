{-# LANGUAGE ExplicitNamespaces #-}
module WikiEL.Util where

import           Data.Vector      ( Vector )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import           WikiEL.Type      ( IRange(..), RelativePosition(..), type WordsHash )

-- old WikiEL.Misc

relativePos :: IRange -> IRange -> RelativePosition
relativePos (IRange lbeg lend) (IRange rbeg rend)
  -- Note ordering is crucial for correct pattern matching; do not change it unless unavoidable.
  | lend <= rbeg = LbeforeR
  | rend <= lbeg = RbeforeL
  | lbeg == rbeg && rend == lend = Coincide
  | lbeg <= rbeg && rend <= lend = RinL
  | rbeg <= lbeg && lend <= rend = LinR
  | rbeg < lbeg &&  rend < lend = RoverlapL
  | lbeg < rbeg &&  lend < rend = LoverlapR
  | otherwise = error "Logical bug in nextIRange"

untilNoOverlap :: (a->RelativePosition) -> [a] -> [a]
untilNoOverlap _ [] = []
untilNoOverlap f ranges@(r:rs) | LbeforeR == f r = ranges
                               | otherwise       = untilNoOverlap f rs

-- untilNoOverlap f (_:rs) = untilNoOverlap f rs


untilOverlapOrNo :: (a->RelativePosition) -> [a] -> [a]
untilOverlapOrNo _ [] = []
untilOverlapOrNo f ranges@(r:rs) = case f r of
  LbeforeR  -> ranges
  LoverlapR -> ranges
  _ -> untilOverlapOrNo f rs


-- Vector algorithms
-- isContain : check whether a slice of a 2nd input vector is a 1st input vector.
isContain :: Eq a => Vector a -> Vector a -> Bool
isContain = f 
  where
    zipEq x y = V.all (uncurry (==)) (V.zip x y)
    f sub vec | V.length sub > V.length vec = False
    f sub vec | zipEq sub vec = True
    f sub vec                 = f sub (V.tail vec)
--
strictSlice :: Eq a => Vector a -> Vector a -> Bool
strictSlice sub vec = isContain sub vec && (V.length sub < V.length vec)

subVector :: IRange -> Vector a -> Vector a
subVector (IRange beg end) = V.slice beg (end-beg)

ithElementOrdering :: Int -> WordsHash -> WordsHash -> Ordering
ithElementOrdering i lhs rhs | UV.length lhs <= i = LT
                             | UV.length rhs <= i = GT
                             | otherwise = compare (lhs UV.! i) (rhs UV.!i)

