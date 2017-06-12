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

mk1HotVec :: Int -> Maybe Int -> Vector CFloat
mk1HotVec dim Nothing = V.replicate dim 0
mk1HotVec dim (Just n) = V.generate dim (\i -> if i == n then 1.0 else 0.0)

pblabel2vec :: PropBankLabel -> Vector CFloat
pblabel2vec = mk1HotVec dim . pblabel2idx
  where dim = fromEnum (maxBound :: LinkType) + fromEnum (maxBound :: ModifierType) + 6

position2vec :: Position -> Vector CFloat
position2vec Before = mk1HotVec 2 (Just 0)
position2vec After  = mk1HotVec 2 (Just 1)
position2vec Embed  = mk1HotVec 2 Nothing

direction2vec :: Direction -> Vector CFloat
direction2vec Up   = mk1HotVec 2 (Just 0)
direction2vec Down = mk1HotVec 2 (Just 1)

enum2vec :: (Bounded a, Enum a) => a -> Vector CFloat
enum2vec x = mk1HotVec dim (Just (fromEnum x))
  where dim = fromEnum (maxBound :: POSTag) + 1

ptp2vec :: ParseTreePath -> Vector CFloat
ptp2vec xs = if n < maxn then v0 V.++ V.replicate (maxn-n) 0 else V.take maxn v0
  where ptp2idx1 (Left  p,d) = enum2vec p V.++ enum2vec d
        ptp2idx1 (Right t,d) = enum2vec t V.++ enum2vec d
        --
        v0 = foldl' (\acc x -> acc V.++ ptp2idx1 x) V.empty xs
        n = V.length v0
        dimc = fromEnum (maxBound :: ChunkTag) + 1
        dimp = fromEnum (maxBound :: POSTag) + 1
        maxn  = 10*(dimc + 2) + dimp+2 

argnode2vec :: ArgNodeFeature -> Maybe (Vector CFloat)
argnode2vec (_arglabel,(_,ptp,Just (_,(_,(pos,_word))))) = 
  let v2 = ptp2vec ptp
      v3 = enum2vec pos
      v = v2 V.++ v3 
  in Just v
argnode2vec (_arglabel,(_,_ptp,Nothing)) = Nothing

 
inst2vec :: InstanceFeature -> [(Int,RoleSet,PropBankLabel,Range,Vector CFloat)]
inst2vec ifeat =
  let predv = enum2vec (ifeat^._3)
      rs = flip map (concat (ifeat^._4)) $ \nfeat -> 
        let n = ifeat^._1
            roleset=  ifeat^._2
            label = nfeat^._1
            rng = nfeat^._2._1
            mvec = argnode2vec nfeat
        in fmap (\v -> (n,roleset,label,rng, predv V.++ v)) mvec
  in catMaybes rs

