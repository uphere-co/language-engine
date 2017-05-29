{-# LANGUAGE OverloadedStrings #-}

module SRL.Util where

import           Data.Text      (Text)
import qualified Data.Text as T (intercalate, unpack)
--
import           NLP.Type.PennTreebankII


clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 

formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)

getLeaves :: PennTreeGen c t a -> [(t,a)]
getLeaves (PN _ xs) = concatMap getLeaves xs
getLeaves (PL t a) = [(t,a)]

findNoneLeaf :: PennTreeGen c Text a -> [(Text,a)]
findNoneLeaf = filter (\(t,_) -> t == "-NONE-") . getLeaves 

x `isInside` (x1,y1) = x1 <= x && x <= y1

(x0,y0) `isInsideR` (x1,y1) = x1 <= x0 && y0 <= y1

extractIndexOut :: PennTreeGen (i1,c) (i2,t) (i3,a) -> PennTreeGen c t a
extractIndexOut (PN (_,c) xs) = PN c (map extractIndexOut xs)
extractIndexOut (PL (_,t) (_,x)) = PL t x 

