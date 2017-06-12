{-# LANGUAGE TemplateHaskell #-}

module SRL.Vectorize.Sparse where

import           Control.Lens
import           Data.List                 (foldl')
import           Data.Maybe                (catMaybes)
import           Data.Vector.Storable      (Vector)
import qualified Data.Vector.Storable as V
import           Foreign.C.Types
--
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
--
import           SRL.Type


pblabel2idx :: PropBankLabel -> Maybe Int
pblabel2idx Relation = Nothing
pblabel2idx (NumberedArgument n) | n <= 4 && n >= 0 = Just n
                                 | otherwise        = Nothing
pblabel2idx (Modifier m) = Just (fromEnum m + 5)
pblabel2idx (LinkArgument l) = Just (fromEnum l + fromEnum (maxBound :: ModifierType) + 6)

data FeatureVector = FV { _fv_dim   :: Int
                        , _fv_nodes :: [(Int,Double)]
                        }
                   deriving Show
emptyFV = FV 0 []

makeLenses ''FeatureVector
                            
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
    
    

    
mkFeatureVector :: Int -> Maybe Int -> FeatureVector -- [(Int,Double)] -- Vector CFloat
mkFeatureVector dim Nothing  = FV dim [] -- V.replicate dim 0
mkFeatureVector dim (Just n) = FV dim [(n,1.0)] -- V.generate dim (\i -> if i == n then 1.0 else 0.0)


pblabel2vec :: PropBankLabel -> FeatureVector -- Vector CFloat
pblabel2vec = mkFeatureVector dim . pblabel2idx
  where dim = fromEnum (maxBound :: LinkType) + fromEnum (maxBound :: ModifierType) + 6

position2vec :: Position -> FeatureVector -- Vector CFloat
position2vec Before = mkFeatureVector 2 (Just 0)
position2vec After  = mkFeatureVector 2 (Just 1)
position2vec Embed  = mkFeatureVector 2 Nothing

direction2vec :: Direction -> FeatureVector -- Vector CFloat
direction2vec Up   = mkFeatureVector 2 (Just 0)
direction2vec Down = mkFeatureVector 2 (Just 1)

enum2vec :: (Bounded a, Enum a) => a -> FeatureVector -- Vector CFloat
enum2vec x = mkFeatureVector dim (Just (fromEnum x))
  where dim = fromEnum (maxBound :: POSTag) + 1

ptp2vec :: ParseTreePath -> FeatureVector -- Vector CFloat
ptp2vec xs = fitToDim maxn v0
  where ptp2idx1 (Left  p,d) = enum2vec p `concatFV` enum2vec d
        ptp2idx1 (Right t,d) = enum2vec t `concatFV` enum2vec d
        --
        v0 = foldl' (\acc x -> acc `concatFV` ptp2idx1 x) emptyFV xs
        n = v0 ^. fv_dim
        dimc = fromEnum (maxBound :: ChunkTag) + 1
        dimp = fromEnum (maxBound :: POSTag) + 1
        maxn  = 10*(dimc + 2) + dimp+2 

argnode2vec :: ArgNodeFeature -> Maybe FeatureVector --  (Vector CFloat)
argnode2vec (_arglabel,(_,ptp,Just (_,(_,(pos,_word))))) = 
  let v2 = ptp2vec ptp
      v3 = enum2vec pos
      v = v2 `concatFV` v3 
  in Just v
argnode2vec (_arglabel,(_,_ptp,Nothing)) = Nothing

 
inst2vec :: InstanceFeature -> [(Int,RoleSet,PropBankLabel,Range,FeatureVector)]
inst2vec ifeat =
  let predv = enum2vec (ifeat^._3)
      rs = flip map (concat (ifeat^._4)) $ \nfeat -> 
        let n = ifeat^._1
            roleset=  ifeat^._2
            label = nfeat^._1
            rng = nfeat^._2._1
            mvec = argnode2vec nfeat
        in fmap (\v -> (n,roleset,label,rng, predv `concatFV` v)) mvec
  in catMaybes rs


