{-# LANGUAGE OverloadedStrings #-}

module NLP.Type.PennTreebankII.Match where

import           Control.Lens
import           Control.Monad.Loops             (unfoldM)
import           Control.Monad.Trans.State
import           Data.Bifoldable                 (bifoldMap)
import           Data.Foldable                   (toList)
-- import           Data.List                       (find,intercalate)
import           Data.Maybe                      (mapMaybe,listToMaybe,maybeToList)
import           Data.Text                       (Text)
--
import           Data.Range                      (isInside,isInsideR)
import           NLP.Type.PennTreebankII
--
{- import           PropBank.Type.Match
import           PropBank.Type.Prop
import           PropBank.Util -}
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


getMerged :: PennTree -> [[(Int,(POSTag,Text))]]
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
  (_,(_,ml)) <- filter ((== i) . fst) i2t
  l <- maybeToList ml
  r <- filter ((== l) . fst) l2p
  return r


exclusionList :: PennTree -> [Int]
exclusionList tr = let f []             = []
                       f ((i,(t,_)):xs) = (if t == D_NONE then [i] else []) ++ map (^._1) xs 
                   in concatMap f (getMerged tr)


convertTop :: PennTree -> PennTree
convertTop (PN _ xs) = PN "ROOT" xs
convertTop x         = x


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



matchR :: Range -> PennTreeIdxG c t -> Maybe (PennTreeIdxG c t)
matchR r0 y@(PN (r,_) xs)
  | r0 == r = Just y 
  | otherwise = listToMaybe (mapMaybe (matchR r0) xs)
matchR (b,e) x@(PL (n,_))
  | b == n && e == n = Just x
  | otherwise = Nothing


