{-# LANGUAGE OverloadedStrings #-}

module PropBank.Util where

import           Data.Discrimination               (leftOuter)
import           Data.Discrimination.Grouping      (grouping)
import           Data.Function                     (on)
import           Data.List                         (groupBy)
import           Data.Text      (Text)
--
import           NLP.Type.PennTreebankII



merge :: (b -> Int) -> [a] -> [b] -> [(Int,(a,[b]))] 
merge idxf trs props =
   let itrs = (zip [0..] trs)
       propgrps = (map (\x->(idxf (head x),x)) . groupBy ((==) `on` idxf)) props
   in concat $ leftOuter grouping joiner m1 fst fst itrs propgrps
 where joiner (i,x) (_,y) = (i,(x,y))
       m1 (i,x) = (i,(x,[]))


getLeaves :: PennTreeGen c t -> [t]
getLeaves (PN _ xs) = concatMap getLeaves xs
getLeaves (PL x) = [x]


findNoneLeaf :: PennTreeGen c (Int,(Text,a)) -> [(Int,(Text,a))]
findNoneLeaf = filter (\(_,(t,_)) -> t == "-NONE-") . getLeaves 

isInside :: Int -> Range -> Bool
x `isInside` (x1,y1) = x1 <= x && x <= y1

isInsideR :: Range -> Range -> Bool
(x0,y0) `isInsideR` (x1,y1) = x1 <= x0 && y0 <= y1
