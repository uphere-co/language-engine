{-# LANGUAGE OverloadedStrings #-}

module PropBank.Util where

import           Data.Discrimination               (leftOuter)
import           Data.Discrimination.Grouping      (grouping)
import           Data.Function                     (on)
import           Data.List                         (groupBy)
--


merge :: (b -> Int) -> [a] -> [b] -> [(Int,(a,[b]))] 
merge idxf trs props =
   let itrs = (zip [0..] trs)
       propgrps = (map (\x->(idxf (head x),x)) . groupBy ((==) `on` idxf)) props
   in concat $ leftOuter grouping joiner m1 fst fst itrs propgrps
 where joiner (i,x) (_,y) = (i,(x,y))
       m1 (i,x) = (i,(x,[]))

