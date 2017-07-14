{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SRL.Util where

import           Control.Lens
import           Data.Bifoldable                             (biList)
import           Data.Discrimination
import           Data.IntMap       (IntMap)
import qualified Data.IntMap as IM 
import           Data.Text         (Text)
import qualified Data.Text   as T  (intercalate)
--
import           Data.Attribute
import           Data.Bitree
import           NLP.Type.PennTreebankII
--
import           SRL.Type


clippedText :: (Int,Int) -> [Text] -> Text
clippedText (b,e) = T.intercalate " " . drop b . take (e+1) 


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


extractIndexOut :: PennTreeGen (i,c) (j,t) -> PennTreeGen c t
extractIndexOut (PN (_,c) xs) = PN c (map extractIndexOut xs)
extractIndexOut (PL (_,x)) = PL x 


findNotOverlappedNodes :: PennTreeIdx -> Range -> [Range]
findNotOverlappedNodes ipt rng = filter (`isNotOverlappedWith` rng)
                               . map (\(PN (r,_) _) -> r)
                               . filter (\case PN _ _ -> True ; _ -> False)
                               . biList
                               . duplicate 
                               $ ipt 



-- Bitree c (Int,t) -> Bitree c (Int,(Maybe v,t))
decorateLeaves :: IntMap v -> PennTreeIdxG n (ALAtt ls) -> PennTreeIdxG n (ALAtt (Maybe v ': ls))
decorateLeaves m tr = let lkup (n,ALeaf t ts) = (n, ALeaf t (IM.lookup n m `acons` ts)) in fmap lkup tr


joiningIntMap :: IntMap v -> IntMap v' -> IntMap (v,v')
joiningIntMap m1 m2 =
  IM.fromList (joining grouping (\as bs -> (fst (head as), (snd (head as),snd (head bs)))) fst fst (IM.toAscList m1) (IM.toAscList m2))


rightOuterIntMap :: IntMap v -> IntMap v' -> IntMap (Maybe v,v')
rightOuterIntMap m1 m2 =
  let l1 = IM.toAscList m1
      l2 = IM.toAscList m2
      rss = rightOuter grouping (\a b->(a^._1, (Just (a^._2), b^._2))) (\b->(b^._1,(Nothing,b^._2))) fst fst l1 l2
  in IM.fromList (concat rss)
