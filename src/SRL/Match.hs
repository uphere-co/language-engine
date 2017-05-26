module SRL.Match where

import           Control.Arrow                     ((***))
import           Control.Lens
import           Data.Maybe                        (fromJust)
--
import           NLP.Type.PennTreebankII
import           PropBank.Parser.Prop
import           PropBank.Type.Prop
--
import           SRL.Util
                 
type Range = (Int,Int)


adjustIndexFromTree :: PennTree -> Int -> Int
adjustIndexFromTree tr =
  let itr = mkIndexedTree tr
      rs = termRangeForAllNode itr
      excl = map (^._2._1) (findNoneLeaf itr)
  in adjustIndex excl 

findMatchedNode :: ((PennTree,PennTree),[Instance]) -> [[(Range,[(Range,PennTree)])]]
findMatchedNode ((pt,tr),prs) = 
  [findMatchedNodeEach (pt,tr) pr arg0 | pr <- prs , arg0 <- pr^.inst_arguments ]

findMatchedNodeEach :: (PennTree,PennTree) -> Instance -> Argument -> [(Range,[(Range,PennTree)])]
findMatchedNodeEach (pt,tr) pr0 arg0 = do
  let nds = map (flip findNode tr) (arg0 ^. arg_terminals)
  nd <- fromJust <$> nds
  let adjf = adjustIndexFromTree tr
      rng = ((adjf *** adjf) . termRange . snd) nd
      xs = termRangeForAllNode (mkIndexedTree pt)
      ipt = mkIndexedTree pt
      zs = maximalEmbeddedRange ipt rng
  return (rng,zs)
