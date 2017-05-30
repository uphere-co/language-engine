module SRL.IdentifyVoice where

import           Data.Either                   (lefts)
import qualified Data.IntMap             as IM
import           Data.Maybe                    (fromJust)
import           Data.Text                     (Text)
import qualified Data.Text               as T
--
import           NLP.Type.PennTreebankII
  
  
ancestorTree :: PennTreeGen c p a -> PennTreeGen c p ([PennTreeGen c p a],PennTreeGen c p a)
ancestorTree = go []
  where
    go xs x@(PN c ys) = PN c (map (go (x:xs)) ys)
    go xs x@(PL p y)  = PL p (xs,x)

ancestorTreeTagOnly :: PennTreeGen c p a -> PennTreeGen c p ([c],PennTreeGen c p a)
ancestorTreeTagOnly = fmap (\(xs, y) -> (lefts (map getTag xs),y)) . ancestorTree

daughters :: PennTreeGen c p a -> [PennTreeGen c p a]
daughters (PN c xs) = xs
daughters (PL p x) = []


data ListZipper a = ListZipper { _lz_before :: [a]
                               , _lz_current :: a
                               , _lz_after :: [a] }
                  deriving Show

                          
                   
siblings :: PennTreeIdxG c p ([PennTreeIdxG c p a],PennTreeIdxG c p a)
         -> PennTreeIdxG c p (ListZipper (PennTreeIdxG c p a))
siblings = trimap id id f
  where
    f (n,(lst,x)) = case lst of
                      []     -> (n, ListZipper [] x []) -- impossible case
                      (y:ys) -> let (zs1,_:zs2) = span (\z -> snd (getRange z) < n) (daughters y)
                                in (n, ListZipper (reverse zs1) x zs2)

lemmatize :: IM.IntMap Text
          -> PennTreeIdxG ChunkTag POSTag Text
          -> PennTreeIdxG ChunkTag POSTag (Text,Text)
lemmatize m = trimap id id f where f (i,t) = (i,(t,fromJust (IM.lookup i m)))

{- 
type PTreeIdxA = PennTreeIdxG ChunkTag POSTag
-}

{- 
rule1 :: (POSTag,(Int,Text))
      -> [PTreeIdxA ([PTreeIdxA (Text,Text)],(Text,Text))]
      -> Bool
rule1 _ []     = False
rule1 v (x:xs)
  | v^._1 == VBN = case x of
                     PN VP ys -> let n = v^._1._1
                                     (prev,_)  = span (\x -> x < n) ys
                                 in case reverse prev of
                                      [] -> False
                                      (y:_) -> case y of
                                               
                     _ -> False
  | otherwise    = False             
                   
-}
