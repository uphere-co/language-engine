module SRL.Util where

import           Data.Foldable               (toList)
import           Data.Function               (on)
import           Data.List                   (sortBy)
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII

minimalSpanNode :: (Int,Int) -> PennTree -> PennTree
minimalSpanNode = undefined


termRange :: PennTreeGen c t (Int,a) -> (Int,Int)
termRange tr = let is = (map fst . toList) tr 
               in (minimum is,maximum is)


termRangeForAllNode :: PennTreeGen c t (Int,a) -> [(Int,Int)]
termRangeForAllNode x@(PN _ ys) = termRange x : concatMap termRangeForAllNode ys
termRangeForAllNode (PL _ (i,_)) = [(i,i)]
