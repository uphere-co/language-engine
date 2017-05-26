{-# LANGUAGE OverloadedStrings #-}

module SRL.Util where

import           Control.Lens
import           Data.Foldable                   (toList)
import           Data.Function                   (on)
import           Data.List                       (sortBy)
import           Data.Maybe                      (fromJust,maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util


type Range = (Int,Int)


clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 

formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)

termRange :: PennTreeGen c t (Int,a) -> Range
termRange tr = let is = (map fst . toList) tr 
               in (minimum is,maximum is)


termRangeForAllNode :: PennTreeGen c t (Int,a) -> [Range]
termRangeForAllNode x@(PN _ ys) = termRange x : concatMap termRangeForAllNode ys
termRangeForAllNode (PL _ (i,_)) = [(i,i)]


getLeaves :: PennTreeGen c t a -> [(t,a)]
getLeaves (PN _ xs) = concatMap getLeaves xs
getLeaves (PL t a) = [(t,a)]

findNoneLeaf :: PennTreeGen c Text a -> [(Text,a)]
findNoneLeaf = filter (\(t,_) -> t == "-NONE-") . getLeaves 

adjustIndex :: [Int] -> Int -> Either Int Int
adjustIndex xs n = let m = length (filter (<n) xs)
                   in if n `elem` xs then Left (n-m) else Right (n-m)

adjustIndexFromTree :: PennTree -> Int -> Either Int Int
adjustIndexFromTree tr =
  let itr = mkIndexedTree tr
      rs = termRangeForAllNode itr
      excl = map (^._2._1) (findNoneLeaf itr)
  in adjustIndex excl 


termRangeTree :: PennTreeGen c t (Int,a) -> PennTreeGen (c,Range) (t,Range) (Int,a)
termRangeTree tr@(PN c xs) = let is = (map fst . toList) tr 
                                 rng = (minimum is,maximum is)
                             in PN (c,rng) (map termRangeTree xs)
termRangeTree (PL t (n,x))     = PL (t,(n,n)) (n,x)

(x0,y0) `isInside` (x1,y1) = x1 <= x0 && y0 <= y1


maximalEmbeddedRange :: PennTreeGen c t (Int,a) -> Range -> [(Range,PennTreeGen c t a)]
maximalEmbeddedRange tr r = go trt
  where trt = termRangeTree tr

        go y@(PN (c,r1) xs) = if r1 `isInside` r then [(r1,extractIndexOut y)] else concatMap go xs
        go y@(PL (t,r1) x) = if r1 `isInside` r then [(r1,extractIndexOut y)] else []

extractIndexOut :: PennTreeGen (c,i1) (t,i2) (i3,a) -> PennTreeGen c t a
extractIndexOut (PN (c,_) xs) = PN c (map extractIndexOut xs)
extractIndexOut (PL (t,_) (_,x)) = PL t x 


findMatchedNode :: ((PennTree,PennTree),[Instance]) -> [[((Range,Argument),[(Range,PennTree)])]]
findMatchedNode ((pt,tr),prs) = 
  [findMatchedNodeEach (pt,tr) pr arg0 | pr <- prs , arg0 <- pr^.inst_arguments ]

findMatchedNodeEach :: (PennTree,PennTree) -> Instance -> Argument
                    -> [((Range,Argument),[(Range,PennTree)])]
findMatchedNodeEach (pt,tr) pr arg = do
  let nds = map (flip findNode tr) (arg ^. arg_terminals)
  nd <- fromJust <$> nds
  let adjf = adjustIndexFromTree tr
      adjrange (x,y) = case (adjf x, adjf y) of
                         (Left b,Left e) -> if b == e then [] else [(b,e)]
                         (Left b,Right e) -> [(b,e)]
                         (Right b,Left e) -> [(b,e-1)]
                         (Right b,Right e) -> [(b,e)]
  rng <- (adjrange . termRange . snd) nd
  let xs = termRangeForAllNode (mkIndexedTree pt)
      ipt = mkIndexedTree pt
      zs = maximalEmbeddedRange ipt rng
  return ((rng,arg),zs)

