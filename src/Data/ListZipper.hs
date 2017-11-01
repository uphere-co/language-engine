{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.ListZipper where

import           Control.Lens
import           Data.List     (unfoldr)


data ListZipper a = LZ { _lz_prevs :: [a]
                       , _lz_current :: a
                       , _lz_nexts :: [a]
                       }
                  deriving (Show,Eq,Ord,Functor)

makeLenses ''ListZipper


singletonLZ :: a -> ListZipper a
singletonLZ x = LZ [] x []

nextLZ :: ListZipper a -> Maybe (ListZipper a)
nextLZ (LZ ps c (n:ns)) = Just (LZ (c:ps) n ns)
nextLZ _                = Nothing


lzToList :: ListZipper a -> [a]
lzToList x = x^.lz_prevs.to reverse ++ (x^.lz_current : x^.lz_nexts)


replaceLZ :: a -> ListZipper a -> ListZipper a
replaceLZ y (LZ ps _ ns) = LZ ps y ns


mergeLeftLZ :: ListZipper a -> ListZipper a -> ListZipper a
mergeLeftLZ (LZ ps x ns) l2 = LZ  ps x (ns ++ (lzToList l2))


mergeRightLZ :: ListZipper a -> ListZipper a -> ListZipper a
mergeRightLZ l1 (LZ ps x ns) = LZ (ps++reverse (lzToList l1)) x ns


genListZippers :: [a] -> [ListZipper a]
genListZippers [] = []
genListZippers (k:ks) = l1 : unfoldr suc l1
  where l1 = LZ [] k ks
        suc (LZ _xs _y []    ) = Nothing
        suc (LZ xs  y  (z:zs)) = let w = LZ (y:xs) z zs in Just (w,w)

