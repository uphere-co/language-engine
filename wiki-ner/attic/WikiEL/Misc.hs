module WikiEL.Misc where

import qualified Data.Vector                   as V
import           Data.Vector                           (Vector)
--
import           WikiEL.Type                           (IRange(..),RelativePosition(..))


untilOverlapOrNo :: (a->RelativePosition) -> [a] -> [a]
untilOverlapOrNo _ [] = []
untilOverlapOrNo f ranges@(r:rs) = case f r of
  LbeforeR  -> ranges
  LoverlapR -> ranges
  _ -> untilOverlapOrNo f rs


