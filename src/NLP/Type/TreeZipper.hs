{-# LANGUAGE TemplateHaskell #-}

module NLP.Type.TreeZipper where

import           Data.List               (unfoldr)
--
import           NLP.Type.PennTreebankII
--
import Debug.Trace

type Tree = PennTreeGen 

-- | Tree Zipper                 
data TreeZipper c t = TZ (Tree c t) [(c,[Tree c t],[Tree c t])]
  deriving (Show)
 
mkListZipper :: [a] -> [([a],a,[a])]
mkListZipper [] = error "cannot make a zipper for empty list"
mkListZipper (k:ks) = ([],k,ks) : unfoldr succ ([],k,ks)
  where succ (xs,y,[]) = Nothing
        succ (xs,y,z:zs) = let w = (y:xs,z,zs) in Just (w,w)

mkTreeZipper :: [(c,[Tree c t],[Tree c t])] -> Tree c t -> Tree (TreeZipper c t) (TreeZipper c t)
mkTreeZipper zs p@(PL x) = PL (TZ p zs)
mkTreeZipper zs p@(PN x xs) = PN (TZ p zs) lst
  where lst = map (\(xs1,y,xs2) -> mkTreeZipper ((x,xs1,xs2):zs) y) (mkListZipper xs)


current :: TreeZipper c t -> Tree c t
current (TZ x _) = x

prev :: TreeZipper c t -> Maybe (TreeZipper c t)
prev (TZ x (y:ys)) = case y of
                       (c,z:zs,ws) -> Just (TZ z ((c,zs,x:ws):ys))
                       _           -> Nothing
prev _             = Nothing
                              
next :: TreeZipper c t -> Maybe (TreeZipper c t)
next (TZ x (y:ys)) = case y of
                       (c,zs,w:ws) -> Just (TZ w ((c,x:zs,ws):ys))
                       _           -> Nothing
next _             = Nothing      

parent :: TreeZipper c t -> Maybe (TreeZipper c t)
parent (TZ x (y:ys)) =
  case y of
    (c,zs,ws) -> Just (TZ (PN c (f (x:ws))) ys)
      where f = foldr (\x acc -> acc . (x:)) id zs 
parent _             = Nothing 

 
child1 :: TreeZipper c t -> Maybe (TreeZipper c t)
child1 (TZ (PL _) _) = Nothing
child1 (TZ (PN c (x:xs)) ys) = Just (TZ x ((c,[],xs):ys))

