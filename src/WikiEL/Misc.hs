{-# LANGUAGE DeriveGeneric              #-}

module WikiEL.Misc where

import           Data.Aeson
import qualified Data.Vector                   as V
import           Data.Vector                           (Vector,toList,fromList,ifoldr,foldl')
import           GHC.Generics                          (Generic)

data IRange = IRange { beg :: Int
                     , end :: Int}
                deriving(Eq,Generic)

instance ToJSON IRange where
  toJSON = genericToJSON defaultOptions

instance FromJSON IRange where
  parseJSON = genericParseJSON defaultOptions

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
