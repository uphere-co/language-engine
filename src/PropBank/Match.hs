{-# LANGUAGE OverloadedStrings #-}

module PropBank.Match where

import           Control.Lens
import           Data.List                       (find)
import           Data.Maybe                      (listToMaybe,maybeToList)
--
import           NLP.Type.PennTreebankII
import           NLP.Type.PennTreebankII.Match
--
import           PropBank.Type.Match
import           PropBank.Type.Prop
--


findNode :: Node -> PennTreeGen c (Int,(p,t)) -> Maybe (p, PennTreeGen c (Int,(p,t)))
findNode (Node i d) itr = do
  let lst = reverse (contain i itr)
  PL (_,(headword,_)) <- listToMaybe (take 1 lst)
  r <- listToMaybe $ drop d lst
  return (headword,r)


matchArgNodes :: (PennTree,PennTree)  -- ^ (CoreNLP tree, PropBank (human-annotated) tree)
              -> Argument
              -> [MatchedArgNode]
matchArgNodes (coretr,proptr) arg = do
    n <- arg ^. arg_terminals
    let iproptr = mkIndexedTree proptr
    nd <- maybeToList (findNode n iproptr)
    rng <- case nd of
             ("-NONE-",PL (_,_)) -> adjrange =<< map snd (findLinks l2p i2t 34)
             _                   -> (adjrange . termRange . snd) nd
    let zs = maximalEmbeddedRange ipt rng
    return MatchedArgNode { _mn_node = (rng,n), _mn_trees = zs }

  where
    l2p = linkID2PhraseNode proptr
    i2t = index2TraceLinkID (mkPennTreeIdx proptr)
    adjf = adjustIndexFromTree proptr
    adjrange (x,y) = case (adjf x, adjf y) of
                       (Left b,Left e) -> if b == e then [] else [(b,e)]
                       (Left b,Right e) -> [(b,e)]
                       (Right b,Left e) -> [(b,e-1)]
                       (Right b,Right e) -> [(b,e)]
    ipt = (mkIndexedTree . getADTPennTree) coretr


matchArgs :: (PennTree,PennTree) -> Instance -> [MatchedArgument]
matchArgs (coretr,proptr) pr
  = [ MatchedArgument { _ma_argument = a, _ma_nodes = matchArgNodes (coretr,proptr) a } | a <- pr^.inst_arguments ]


matchInstances :: (PennTree,PennTree) -> [Instance] -> [MatchedInstance]
matchInstances (coretr,proptr) insts
  = [ MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (coretr,proptr) inst } | inst <- insts ]


findRelNode :: [MatchedArgument] -> Maybe Int
findRelNode args = do
    a1 <- find (\a -> a ^. ma_argument.arg_label == Relation) args
    listToMaybe (a1^..ma_nodes.traverse.mn_node._1._1)


