{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module SRL.Old.Vectorize.Sparse where

import           Control.Lens
import           Data.List                 (foldl')
import           Data.Maybe                (catMaybes)
--
import           Data.ListZipper           (ListZipper,lz_prevs,lz_current,lz_nexts)
import           NLP.Type.PennTreebankII
import           NLP.Type.UniversalDependencies2.Syntax
import           PropBank.Type.Prop
--
import           SRL.Old.Type
--


data FeatureVector = FV { _fv_dim   :: Int
                        , _fv_nodes :: [(Int,Double)]
                        }
                   deriving Show

makeLenses ''FeatureVector


emptyFV :: FeatureVector
emptyFV = FV 0 []

  
fitToDim :: Int -> FeatureVector -> FeatureVector
fitToDim maxn (FV n xs) =
  if n < maxn then FV maxn xs else FV maxn (filter ((<maxn) . fst) xs)


concatFV :: FeatureVector -> FeatureVector -> FeatureVector
concatFV fv1 fv2 = FV (n1+n2) (x1s ++ x2s)
  where
    n1 = fv1^.fv_dim
    n2 = fv2^.fv_dim
    x1s = fv1^.fv_nodes
    x2s = map (\(i,v) -> (i+n1,v)) (fv2^.fv_nodes)
    

mkFeatureVector :: Int -> Maybe Int -> FeatureVector
mkFeatureVector dim Nothing  = FV dim []
mkFeatureVector dim (Just n) = FV dim [(n,1.0)]



pblabel2idx :: PropBankLabel -> Maybe Int
pblabel2idx Relation = Nothing
pblabel2idx (NumberedArgument n) | n <= 4 && n >= 0 = Just n
                                 | otherwise        = Nothing
pblabel2idx (Modifier m) = Just (fromEnum m + 5)
pblabel2idx (LinkArgument l) = Just (fromEnum l + fromEnum (maxBound :: ModifierType) + 6)


pblabel2vec :: PropBankLabel -> FeatureVector
pblabel2vec = mkFeatureVector dim . pblabel2idx
  where dim = fromEnum (maxBound :: LinkType) + fromEnum (maxBound :: ModifierType) + 6

position2vec :: Position -> FeatureVector
position2vec Before = mkFeatureVector 2 (Just 0)
position2vec After  = mkFeatureVector 2 (Just 1)
position2vec Embed  = mkFeatureVector 2 Nothing

direction2vec :: Direction -> FeatureVector
direction2vec Up   = mkFeatureVector 2 (Just 0)
direction2vec Down = mkFeatureVector 2 (Just 1)

enum2vec :: (Bounded a, Enum a) => a -> FeatureVector
enum2vec x = mkFeatureVector dim (Just (fromEnum x))
  where dim = fromEnum (maxBound :: POSTag) + 1

ptp2vec :: ParseTreePath -> FeatureVector
ptp2vec xs = let (us,ds) = span (\(_,d) -> d == Up) xs
             in case us of
                  (Right p,_):us' ->
                    let v1 = fitToDim maxn1 (foldtag (enum2vec p) us')
                        v2 = fitToDim maxn2 (foldtag emptyFV ds)
                    in v1 `concatFV` v2
                  _ -> fitToDim (maxn1+maxn2) emptyFV
  where dimc = fromEnum (maxBound :: ChunkTag) + 1
        dimp = fromEnum (maxBound :: POSTag) + 1
        maxn1 = 9*dimc+dimp
        maxn2 = 10*dimc
        -- there is a case with last one is POSTag. need to find that error case.
        foldtag = foldl' (\(!acc) (x,_) -> case x of {Left t -> acc `concatFV` enum2vec t;Right p -> acc `concatFV` enum2vec p})
        

drp2vec :: ListZipper DepInfo -> FeatureVector
drp2vec drp = let v1 = fitToDim maxn1 (foldtag emptyFV (reverse (drp^..lz_prevs.traverse.dinfo_rel)))
                  v2 = enum2vec (drp^.lz_current.dinfo_rel)
                  v3 = fitToDim maxn2 (foldtag emptyFV (drp^..lz_nexts.traverse.dinfo_rel))
              in v1 `concatFV` v2 `concatFV` v3
  where dim = fromEnum (maxBound :: DependencyRelation) + 1
        maxn1 = 10*dim
        maxn2 = 10*dim
        foldtag = foldl' (\(!acc) x -> acc `concatFV` enum2vec x)
        

argnode2vec :: ArgNodeFeature -> Maybe FeatureVector
argnode2vec (AFeat _arglabel (SRLFeat _ ptp mdrp (Just (_,(_,(pos,_word)))))) = 
  let v1 = ptp2vec ptp
      v2 = enum2vec pos
  in case mdrp of
       Nothing -> Nothing
       Just drp -> let v3 = drp2vec drp in Just (v1 `concatFV` v2 `concatFV` v3)
argnode2vec (AFeat _arglabel (SRLFeat _ _ptp _mdrp Nothing)) = Nothing

 
inst2vec :: InstanceFeature -> [(Int,RoleSet,PropBankLabel,Range,FeatureVector)]
inst2vec ifeat =
  let predv = enum2vec (ifeat^.ifeat_voice)
      rs = flip map (concat (ifeat^.ifeat_afeatss)) $ \afeat -> 
        let n = ifeat^.ifeat_predidx
            roleset=  ifeat^.ifeat_rolesetid
            label = afeat^.afeat_label
            rng = afeat^.afeat_srlfeature.sfeat_range
            mvec = argnode2vec afeat
        in fmap (\v -> (n,roleset,label,rng, predv `concatFV` v)) mvec
  in catMaybes rs

