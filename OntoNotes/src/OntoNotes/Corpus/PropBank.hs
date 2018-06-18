{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module OntoNotes.Corpus.PropBank where

import           Control.Lens
import           Data.Function
import           Data.List
--
import           NLP.Type.PennTreebankII


data MatchResult = ExactMatch Range
                 | MergeMatch [Range]
                 | Unmatched
                 deriving Show


newtype NoOverlapSegments = NoOverlapSegments [Range]
                          deriving Show


mkNoOverlapSegments :: [Range] -> Maybe NoOverlapSegments
mkNoOverlapSegments [] = Just (NoOverlapSegments [])
mkNoOverlapSegments (x:xs) =
  let ys = sortBy (compare `on` fst) (x:xs)
      zs = zip ys (tail ys)
  in fmap (NoOverlapSegments . (head ys :))
          (mapM (\((_i,j),(i',j')) -> if j < i' then Just (i',j') else Nothing) zs)


newtype ContiguousSegments = ContiguousSegments [[Range]]
                           deriving Show


-- the implementation is not very good. I will change it.
mkContiguousSegments :: NoOverlapSegments -> ContiguousSegments
mkContiguousSegments (NoOverlapSegments xs) = ContiguousSegments (go [] [] xs)
  where go deck   acc []     = acc ++ [reverse deck]
        go []     acc (r:rs) = go [r] acc rs
        go (z:zs) acc (r:rs) = let ( i , _j ) = r
                                   (_i',  j') = z
                               in if j'+1 == i
                                  then go (r:z:zs) acc          rs
                                  else go [r]      (acc ++ [reverse (z:zs)]) rs


contiguousMatch :: ContiguousSegments -> Range -> [Range]
contiguousMatch (ContiguousSegments segs) (i,j) = foldMap match segs 
  where match xs = 
          let (_,bs) = break (\x->x^._1 == i) xs
              (es,rs) = break (\x->x^._2 == j) bs
          in case rs of
               []    -> []
               (r:_) -> es++[r]


toMatchResult :: [Range] -> MatchResult
toMatchResult []     = Unmatched
toMatchResult (x:[]) = ExactMatch x
toMatchResult xs     = MergeMatch xs


{- 
matchVerbPropertyWithRelation :: [VerbProperty (BitreeZipperICP '[Lemma])]
                              -> Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
                              -> MatchedInstance
                              -> Maybe (VerbProperty (BitreeZipperICP '[Lemma])
                                       ,Maybe (PredArgWorkspace '[Lemma] (Either (Range,STag) (Int,POSTag))))
matchVerbPropertyWithRelation verbprops clausetr minst = do
  relidx <- findRelNode (minst^.mi_arguments)
  vp <- find (\vp->vp^.vp_index==relidx) verbprops
  let cpstr = (map (bindingAnalysis []) . identifyCPHierarchy []) verbprops   -- for the time being
      mpa = findPAWS [] clausetr vp cpstr                                -- for the time being
  return (vp,mpa)

-}
