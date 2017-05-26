{-# LANGUAGE OverloadedStrings #-}

module SRL.Util where

import           Data.Foldable               (toList)
import           Data.Function               (on)
import           Data.List                   (sortBy)
import           Data.Text                   (Text)
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII


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
