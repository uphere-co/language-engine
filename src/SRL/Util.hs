{-# LANGUAGE OverloadedStrings #-}

module SRL.Util where

import           Control.Lens
import           Data.Foldable                   (toList)
import           Data.Function                   (on)
import           Data.List                       (sortBy)
import           Data.Maybe                      (fromJust,maybeToList)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
import           PropBank.Util


type Range = (Int,Int)

type PennTreeIdxG c t a = PennTreeGen (Range,c) (Range,t) (Int,a)

type PennTreeIdx = PennTreeIdxG Text Text Text

clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 

formatRngText terms p = show p ++ ": " ++ T.unpack (clippedText p terms)


mkPennTreeIdx :: PennTree -> PennTreeIdx
mkPennTreeIdx = termRangeTree . mkIndexedTree

  
  
printMatchedInst ::(Instance,[(Argument,[((Range,Node),[(Range,PennTreeIdx)])])]) -> IO ()
printMatchedInst (inst,lst) = do
  TIO.putStrLn (inst^.inst_lemma_type)
  putStrLn "---"  
  mapM_ printMatchedArg lst
  putStrLn "---"  


printMatchedArg :: (Argument,[((Range,Node),[(Range,PennTreeIdx)])]) -> IO ()
printMatchedArg (arg,lst) = do
  TIO.putStrLn $ arg^.arg_label 
  mapM_ printMatchedNode lst  

printMatchedNode :: ((Range,Node),[(Range,PennTreeIdx)]) -> IO ()
printMatchedNode ((r,_),lst) = do
  TIO.putStrLn $ T.pack (show r) <> ":"
  print lst

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


termRangeTree :: PennTreeGen c t (Int,a) -> PennTreeIdxG c t a 
termRangeTree tr@(PN c xs) = let is = (map fst . toList) tr 
                                 rng = (minimum is,maximum is)
                             in PN (rng,c) (map termRangeTree xs)
termRangeTree (PL t (n,x))     = PL ((n,n),t) (n,x)

x `isInside` (x1,y1) = x1 <= x && x <= y1

(x0,y0) `isInsideR` (x1,y1) = x1 <= x0 && y0 <= y1


maximalEmbeddedRange :: PennTreeGen c t (Int,a) -> Range -> [(Range,PennTreeIdxG c t a)]
maximalEmbeddedRange tr r = go (termRangeTree tr)
  where go y@(PN (r1,c) xs) = if r1 `isInsideR` r then [(r1,y)] else concatMap go xs
        go y@(PL (r1,t) x) = if r1 `isInsideR` r then [(r1,y)] else []


extractIndexOut :: PennTreeGen (i1,c) (i2,t) (i3,a) -> PennTreeGen c t a
extractIndexOut (PN (_,c) xs) = PN c (map extractIndexOut xs)
extractIndexOut (PL (_,t) (_,x)) = PL t x 


matchInstances :: (PennTree,PennTree)
               -> [Instance]
               -> [(Instance,[(Argument,[((Range,Node),[(Range,PennTreeIdx)])])])]
matchInstances (pt,tr) prs = [(pr,matchInstArgs (pt,tr) pr) | pr <- prs ]

matchInstArgs :: (PennTree,PennTree) -> Instance -> [(Argument,[((Range,Node),[(Range,PennTreeIdx)])])]
matchInstArgs (pt,tr) pr = [(arg,matchArgNodes (pt,tr) arg) | arg <- pr^.inst_arguments ]

matchArgNodes :: (PennTree,PennTree) -> Argument -> [((Range,Node),[(Range,PennTreeIdx)])]
matchArgNodes (pt,tr) arg = do
        n <- arg ^. arg_terminals
        let nd = fromJust (findNode n tr)
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
        return ((rng,n),zs)

