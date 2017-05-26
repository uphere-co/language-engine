{-# LANGUAGE OverloadedStrings #-}

module SRL.Util where

import           Data.Foldable                   (toList)
import           Data.Function                   (on)
import           Data.List                       (sortBy)
import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII


clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 

formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)

termRange :: PennTreeGen c t (Int,a) -> (Int,Int)
termRange tr = let is = (map fst . toList) tr 
               in (minimum is,maximum is)


termRangeForAllNode :: PennTreeGen c t (Int,a) -> [(Int,Int)]
termRangeForAllNode x@(PN _ ys) = termRange x : concatMap termRangeForAllNode ys
termRangeForAllNode (PL _ (i,_)) = [(i,i)]


getLeaves :: PennTreeGen c t a -> [(t,a)]
getLeaves (PN _ xs) = concatMap getLeaves xs
getLeaves (PL t a) = [(t,a)]

findNoneLeaf :: PennTreeGen c Text a -> [(Text,a)]
findNoneLeaf = filter (\(t,_) -> t == "-NONE-") . getLeaves 

adjustIndex :: [Int] -> Int -> Int
adjustIndex xs n = let m = length (filter (<n) xs) in n-m


termRangeTree :: PennTreeGen c t (Int,a) -> PennTreeGen (c,(Int,Int)) (t,(Int,Int)) (Int,a)
termRangeTree tr@(PN c xs) = let is = (map fst . toList) tr 
                                 rng = (minimum is,maximum is)
                             in PN (c,rng) (map termRangeTree xs)
termRangeTree (PL t (n,x))     = PL (t,(n,n)) (n,x)

(x0,y0) `isInside` (x1,y1) = x1 <= x0 && y0 <= y1


maximalEmbeddedRange :: PennTreeGen c t (Int,a) -> (Int,Int) -> [((Int,Int),PennTreeGen c t a)]
maximalEmbeddedRange tr r = go trt
  where trt = termRangeTree tr

        go y@(PN (c,r1) xs) = if r1 `isInside` r then [(r1,extractIndexOut y)] else concatMap go xs
        go y@(PL (t,r1) x) = if r1 `isInside` r then [(r1,extractIndexOut y)] else []

extractIndexOut :: PennTreeGen (c,i1) (t,i2) (i3,a) -> PennTreeGen c t a
extractIndexOut (PN (c,_) xs) = PN c (map extractIndexOut xs)
extractIndexOut (PL (t,_) (_,x)) = PL t x 

