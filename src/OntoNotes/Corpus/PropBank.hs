{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module OntoNotes.Corpus.PropBank where

import           Control.Lens
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text               as T
import           Text.Printf
--
import           NLP.Syntax.Clause
import           NLP.Syntax.Type
import           NLP.Type.PennTreebankII
import           PropBank.Format
import           PropBank.Match
import           PropBank.Type.Match
import           PropBank.Type.Prop


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



matchVerbPropertyWithRelation :: [VerbProperty (BitreeZipperICP '[Lemma])]
                              -> Bitree (Range,(STag,Int)) (Either (Range,(STag,Int)) (Int,(POSTag,Text)))
                              -> MatchedInstance
                              -> Maybe (VerbProperty (BitreeZipperICP '[Lemma]),Maybe (VerbArgs (Either (Range,STag) (Int,POSTag))))
matchVerbPropertyWithRelation verbprops clausetr minst = do
  relidx <- findRelNode (minst^.mi_arguments)
  vp <- find (\vp->vp^.vp_index==relidx) verbprops
  let mva = getVerbArgs clausetr vp
  return (vp,mva)


formatMatchedVerb :: MatchedInstance
                  -> Maybe (VerbProperty (BitreeZipperICP '[Lemma]), Maybe (VerbArgs (Either (Range,STag) (Int,POSTag))))
                  -> String     
formatMatchedVerb minst mvpmva =                  
  let inst = minst^.mi_instance
      args = filter (\a->a^.ma_argument.arg_label /= Relation) (minst^.mi_arguments)
      header_str = "*************\n"
                   ++ T.unpack (formatRoleSetID (inst^.inst_lemma_roleset_id))
      content_str
        = flip (maybe "unmatched!\n") mvpmva $ \(_vp,mva) ->
            flip (maybe "argument unmatched!\n") mva $ \va -> 
              let vargs = maybeToList (va^.va_arg0) ++ va^.va_args
              in "relation matched\n" ++
                    (intercalate "\n" . flip map args $ \arg -> 
                      let ns = arg^..ma_nodes.traverse.mn_node._1
                          getRng = either (^._1) (\x->(x^._1,x^._1)) 
                          nosegs = fromJust (mkNoOverlapSegments (map getRng vargs))
                      in printf "%15s : %s"
                           (arg^.ma_argument.arg_label.to pbLabelText)              
                           (show (zip ns (map (toMatchResult . contiguousMatch (mkContiguousSegments nosegs)) ns))))
  in header_str ++ "\n" ++ content_str



