module Misc where

import qualified Data.Vector                   as V
import           Data.Vector                           (Vector,toList,fromList,ifoldr,foldl')


data IRange = IRange { beg :: Int
                     , end :: Int}
                deriving(Eq)

instance Show IRange where
  show (IRange beg end) = "IRange [" ++ show beg ++ "," ++ show end ++ ")"


data RelativePosition = LbeforeR | RbeforeL | Coincide | RinL | LinR | LoverlapR | RoverlapL
                      deriving(Show,Eq)

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
untilNoOverlap f ranges@(r:_) | LbeforeR == f r = ranges
untilNoOverlap f ranges@(_:rs) = untilNoOverlap f rs

untilOverlapOrNo :: (a->RelativePosition) -> [a] -> [a]
untilOverlapOrNo _ [] = []
untilOverlapOrNo f ranges@(r:rs) = case f r of
  LbeforeR  -> ranges
  LoverlapR -> ranges
  _ -> untilOverlapOrNo f rs


-- Vector algorithms
isContain :: Eq a => Vector a -> Vector a -> Bool
isContain = f 
  where
    zipEq x y = V.all (uncurry (==)) (V.zip x y)
    f sub vec | V.length sub > V.length vec = False
    f sub vec | zipEq sub vec = True
    f sub vec                 = f sub (V.tail vec)

subVector :: IRange -> Vector a -> Vector a
subVector (IRange beg end) = V.slice beg (end-beg)