{-# LANGUAGE OverloadedStrings #-}

module SRL.Util where

import           Data.Text      (Text)
import qualified Data.Text as T (intercalate, unpack)
--
import           NLP.Type.PennTreebankII


clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 

formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)


safeHead [] = Nothing
safeHead (x:_) = Just x


getLeaves :: PennTreeGen c t -> [t]
getLeaves (PN _ xs) = concatMap getLeaves xs
getLeaves (PL x) = [x]

{- 
getLeavesI :: PennTreeIdxG c t -> [(Int,t)]
getLeavesI (PN _ xs) = concatMap getLeavesI xs
getLeavesI (PL (n,x)) = [(n,x)]
-}


findNoneLeaf :: PennTreeGen c (Int,(Text,a)) -> [(Int,(Text,a))]
findNoneLeaf = filter (\(_,(t,_)) -> t == "-NONE-") . getLeaves 

x `isInside` (x1,y1) = x1 <= x && x <= y1

(x0,y0) `isInsideR` (x1,y1) = x1 <= x0 && y0 <= y1

extractIndexOut :: PennTreeGen (i,c) (j,t) -> PennTreeGen c t
extractIndexOut (PN (_,c) xs) = PN c (map extractIndexOut xs)
extractIndexOut (PL (_,x)) = PL x 

