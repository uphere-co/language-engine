{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.Type.TreeZipper where

import           Data.List               (unfoldr)
--
import           NLP.Type.PennTreebankII
--
import Debug.Trace

type Tree = PennTreeGen 

-- | Surrounding context of the focused item at current level 
data TreeContext c t = TC { _tc_node_content :: c
                          , _tc_prevs :: [Tree c t]
                          , _tc_nexts :: [Tree c t]
                          }
                     deriving (Show)

-- | Zipper for tree                 
data TreeZipper c t = TZ { _tz_current  :: Tree c t          -- ^ current item
                         , _tz_contexts :: [TreeContext c t] -- ^ recusively defined
                                                             --   contexts of current item
                         }
                    deriving (Show)



data ListZipper a = LZ { _lz_prevs :: [a]
                       , _lz_current :: a
                       , _lz_nexts :: [a]
                       }
                  deriving (Show,Functor)

                           
genListZippers :: [a] -> [ListZipper a]
genListZippers [] = error "cannot make a zipper for empty list"
genListZippers (k:ks) = l1 : unfoldr succ l1
  where l1 = LZ [] k ks 
        succ (LZ xs y [])     = Nothing
        succ (LZ xs y (z:zs)) = let w = LZ (y:xs) z zs in Just (w,w)

mkTreeZipper :: [TreeContext c t] -> Tree c t -> Tree (TreeZipper c t) (TreeZipper c t)
mkTreeZipper zs p@(PL x)    = PL (TZ p zs)
mkTreeZipper zs p@(PN x xs) = PN (TZ p zs) lst
  where lst = map (\(LZ xs1 y xs2) -> mkTreeZipper ((TC x xs1 xs2):zs) y) (genListZippers xs)


current :: TreeZipper c t -> Tree c t
current (TZ x _) = x

prev :: TreeZipper c t -> Maybe (TreeZipper c t)
prev (TZ x (y:ys)) = case y of
                       TC c (z:zs) ws -> Just (TZ z ((TC c zs (x:ws)):ys))
                       _              -> Nothing
prev _             = Nothing
                              
next :: TreeZipper c t -> Maybe (TreeZipper c t)
next (TZ x (y:ys)) = case y of
                       TC c zs (w:ws) -> Just (TZ w ((TC c (x:zs) ws):ys))
                       _              -> Nothing
next _             = Nothing      

parent :: TreeZipper c t -> Maybe (TreeZipper c t)
parent (TZ x (y:ys)) =
  case y of
    TC c zs ws -> Just (TZ (PN c (f (x:ws))) ys)
      where f = foldr (\x acc -> acc . (x:)) id zs 
parent _             = Nothing 

 
child1 :: TreeZipper c t -> Maybe (TreeZipper c t)
child1 (TZ (PL _) _) = Nothing
child1 (TZ (PN c (x:xs)) ys) = Just (TZ x ((TC c [] xs):ys))

