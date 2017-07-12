{-# LANGUAGE OverloadedStrings #-}

module PropBank.Match where

import           Control.Applicative             (many)
import           Control.Lens
import           Control.Monad.Loops             (unfoldM)
import           Control.Monad.Trans.State
import qualified Data.Attoparsec.Text       as A
import           Data.Bifoldable                 (bifoldMap)
import           Data.Foldable                   (toList)
import           Data.List (intercalate)
import           Data.Maybe                      (fromJust,mapMaybe,listToMaybe,maybeToList)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                  as T
--
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
--
import           PropBank.Parser.Prop
import           PropBank.Type.Match
import           PropBank.Type.Prop
import           PropBank.Util
--


mergeHyphen :: State [(Int,(POSTag,Text))] (Maybe [(Int,(POSTag,Text))])
mergeHyphen = fmap (fmap reverse) (go Nothing)
  where go acc = do s <- get
                    case s of
                      [] -> return acc
                      (x:xs) -> case acc of
                                  Nothing                    -> put xs >> go (Just [x])
                                  Just ys@((_,(M_HYPH,_)):_) -> put xs >> go (Just (x:ys))
                                  Just ys                    ->
                                    case x of
                                      (_,(M_HYPH,_)) -> put xs >> go (Just (x:ys))
                                      _              -> return acc 


getMerged tr = let atr = (getADTPennTree . convertTop) tr
               in evalState (unfoldM mergeHyphen) (zip [0..] (toList atr))


linkID2PhraseNode :: PennTree -> [(LinkID,Range)]
linkID2PhraseNode tr = 
  let itr = (termRangeTree . mkIndexedTree) tr
  in bifoldMap (\(rng,x) -> [(i,rng) | i <- linkIDChunk x]) (const []) itr


index2TraceLinkID :: PennTreeIdx -> [(Int,(Trace,Maybe LinkID))]
index2TraceLinkID = map f . filter (\(_,(pos,_)) -> pos == D_NONE) . toList
  where f (i,(_,txt)) = (i,identifyTrace txt)


findLinks :: [(LinkID,Range)] -> [(Int,(Trace,Maybe LinkID))] -> Int -> [(LinkID,Range)]
findLinks l2p i2t i = do
  (_,(t,ml)) <- filter ((== i) . fst) i2t
  l <- maybeToList ml
  r <- filter ((== l) . fst) l2p
  return r


exclusionList :: PennTree -> [Int]
exclusionList tr = let f []     = []
                       f ys@((i,(t,_)):xs) = (if t == D_NONE then [i] else []) ++ map (^._1) xs 
                   in concatMap f (getMerged tr)


convertTop :: PennTree -> PennTree
convertTop (PN _ xs) = PN "ROOT" xs


termRangeForAllNode :: PennTreeGen c (Int,t) -> [Range]
termRangeForAllNode x@(PN _ ys) = termRange x : concatMap termRangeForAllNode ys
termRangeForAllNode (PL (i,_)) = [(i,i)]


adjustIndex :: [Int] -> Int -> Either Int Int
adjustIndex xs n = let m = length (filter (<n) xs)
                   in if n `elem` xs then Left (n-m) else Right (n-m)


adjustIndexFromTree :: PennTree -> Int -> Either Int Int
adjustIndexFromTree = adjustIndex . exclusionList


maximalEmbeddedRange :: PennTreeGen c (Int,t) -> Range -> [(Range,PennTreeIdxG c t)]
maximalEmbeddedRange tr r = go (termRangeTree tr)
  where go y@(PN (r1,_) xs) = if r1 `isInsideR` r then [(r1,y)] else concatMap go xs
        go y@(PL (n,_)) = if n `isInside` r then [((n,n),y)] else []


findNodePathForLeaf :: Int -> PennTree -> [PennTreeGen Text (Int,(Text,Text))]
findNodePathForLeaf i tr = contain i (mkIndexedTree tr)


findNode :: Node -> PennTree -> Maybe (Text, PennTreeGen Text (Int,(Text,Text)))
findNode (Node i d) tr = do
  let lst = reverse (findNodePathForLeaf i tr)
  PL (_,(headword,_)) <- listToMaybe (take 1 lst)
  r <- listToMaybe $ drop d lst
  return (headword,r)


matchR :: Range -> PennTreeIdxG c t -> Maybe (PennTreeIdxG c t)
matchR r0 y@(PN (r,_) xs)
  | r0 == r = Just y 
  | otherwise = listToMaybe (mapMaybe (matchR r0) xs)
matchR (b,e) x@(PL (n,_))
  | b == n && e == n = Just x
  | otherwise = Nothing


matchArgNodes :: (PennTree,PennTree)  -- ^ (CoreNLP tree, PropBank (human-annotated) tree)
              -> Argument
              -> [MatchedArgNode]
matchArgNodes (coretr,proptr) arg = do
  n <- arg ^. arg_terminals
  let nd = fromJust (findNode n proptr)
  let adjf = adjustIndexFromTree proptr
      adjrange (x,y) = case (adjf x, adjf y) of
                         (Left b,Left e) -> if b == e then [] else [(b,e)]
                         (Left b,Right e) -> [(b,e)]
                         (Right b,Left e) -> [(b,e-1)]
                         (Right b,Right e) -> [(b,e)]
  rng <- (adjrange . termRange . snd) nd
  let ipt = (mkIndexedTree . getADTPennTree) coretr
      zs = maximalEmbeddedRange ipt rng
  return MatchedArgNode { _mn_node = (rng,n), _mn_trees = zs }


matchArgs :: (PennTree,PennTree) -> Instance -> [MatchedArgument]
matchArgs (pt,tr) pr
  = [ MatchedArgument { _ma_argument = a, _ma_nodes = matchArgNodes (pt,tr) a } | a <- pr^.inst_arguments ]


matchInstances :: (PennTree,PennTree) -> [Instance] -> [MatchedInstance]
matchInstances (pt,tr) insts
  = [ MatchedInstance { _mi_instance = inst, _mi_arguments = matchArgs (pt,tr) inst }
      | inst <- insts ]


findRelNode :: [MatchedArgument] -> Int
findRelNode args =
    let a1 = headf $ filter (\a -> a ^. ma_argument.arg_label == Relation) args
    in headf (a1^..ma_nodes.traverse.mn_node._1._1)
  where headf [] = error ("findRelNode: " ++ intercalate "\n" (map show args))
        headf (x:_) = x




